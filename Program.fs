#if INTERACTIVE
#load ".paket/load/netcoreapp2.2/main.group.fsx"
#load "Window.fs"
#endif


open System
open System.IO.MemoryMappedFiles
open System.IO
open System.Runtime.InteropServices
open FSharp.NativeInterop
open Game
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework
open System.Buffers.Binary
open System.Runtime.CompilerServices
open System.Diagnostics.Tracing
#nowarn "9"



let inline cast (input: 't ReadOnlySpan) =
    MemoryMarshal.Cast(input)

[<Struct>]
type Header =
    { Size: uint32
      Magic: uint16
      Frames: uint16
      Width: uint16
      Height: uint16
      Depth: uint16
      Flags: uint16
      Speed: uint32 }


[<Struct>]
type ViewMemory<'t> = {
        Handle: SafeBuffer
        offset: int
        length: int
} with
        member this.IsEmpty = this.length = 0
        member this.Span() : 't ReadOnlySpan=
                let p = NativePtr.ofNativeInt<byte> (this.Handle.DangerousGetHandle())
                let start = NativePtr.add p this.offset
                
                new ReadOnlySpan<'t>(NativePtr.toVoidPtr start, this.length)
        
        member this.Slice(offset) : ViewMemory<'t> =
                let newOffset = this.offset + offset
                if offset < this.length then
                    { Handle = this.Handle
                      offset = newOffset
                      length = this.length - offset }
                else
                    { Handle = this.Handle
                      offset = this.offset + this.length
                      length = 0 }

        member this.Slice(offset, length) : ViewMemory<'t> =
                let offset = min (this.offset + offset) this.length
                let length = min (this.length - offset) length
                { Handle = this.Handle
                  offset = offset 
                  length = length }
                        
type MemoryMappedViewAccessor with
        member this.AsMemory<'t>(offset, length) =
                { Handle = this.SafeMemoryMappedViewHandle
                  offset = offset
                  length = length //(int this.Capacity - offset)
                } : ViewMemory<'t>
        member this.AsMemory() =
                this.AsMemory(0, int this.Capacity)
        member this.AsMemory(offset) =
                this.AsMemory(offset, int this.Capacity)

[<Struct>]
type FrameHeader = {
    Size: int
    Magic : uint16
    Chuncks: int16
    Reserved: uint64
}

type ChunkType =
    | Rle = 25us
    | Delta = 27us

[<Struct;StructLayout(LayoutKind.Sequential, Pack = 2)>]
type ChunkHeader = {
    Size: int
    Type: ChunkType
}

[<Struct; IsByRefLike>]
type Context =
    { Source: byte ReadOnlySpan
      Dest: uint16 Span }


[<Struct; IsByRefLike>]
type RefTuple<'l> =
    { Value: 'l
      Ctx: Context }

module RefTuple =
    let inline make l ctx = { Value = l; Ctx = ctx}


module Context =
    open System.Runtime.Intrinsics.X86
    let copy size (ctx: Context inref) =
        let source = (cast ctx.Source).Slice(0,size)
        source.CopyTo(ctx.Dest)
        { Source = ctx.Source.Slice(size*2)
          Dest = ctx.Dest.Slice(size) }

    let fill size (ctx: Context inref) =
        let color = uint16 (BinaryPrimitives.ReadInt16LittleEndian ctx.Source)
        let dest = ctx.Dest.Slice(0,size)
        dest.Fill(color)
        { Source = ctx.Source.Slice(2)
          Dest = ctx.Dest.Slice(size)}
        
    let inline readSByte (ctx: Context inref) =
        RefTuple.make 
            (sbyte ctx.Source.[0])
            { ctx with Source = ctx.Source.Slice(1) }

    let inline readByte (ctx: Context inref) =
        RefTuple.make
            (byte ctx.Source.[0])
            { ctx with Source = ctx.Source.Slice(1)  }

    let inline readUInt16 (ctx: Context inref) =
        RefTuple.make
            (uint16 (BinaryPrimitives.ReadInt16LittleEndian ctx.Source))
            { ctx with Source = ctx.Source.Slice(2)  }

    let inline readInt16 (ctx: Context inref) =
        RefTuple.make
            (BinaryPrimitives.ReadInt16LittleEndian ctx.Source)
            { ctx with Source = ctx.Source.Slice(2)  }

    let inline skip src dest (ctx: Context inref) =
        { Source = ctx.Source.Slice(src)
          Dest = ctx.Dest.Slice(dest) }
    
let renderPacket (ctx: Context inref) =
    let r =  Context.readSByte &ctx
    let sizeCount = r.Value
    if sizeCount < 0y then
        let size = int (-sizeCount)
        Context.copy size &r.Ctx
    else
        let count = int sizeCount
        Context.fill count &r.Ctx


let rec renderPackets packets (ctx: Context inref) =
    if packets > 0 then
        let ctx' = renderPacket &ctx
        renderPackets (packets-1) &ctx'
    else
        ctx

let renderLine (ctx: Context inref) =
   let r = Context.readByte &ctx
   let packets = int r.Value
   renderPackets packets &r.Ctx

let rec renderLines height (ctx: Context inref) =
    if height > 0 then
        let ctx' = renderLine &ctx
        renderLines (height-1) &ctx'
    else
        ctx

// let inline renderBRUN height (ctx: Context inref) =
//     renderLines height &ctx


let renderSSHPacket (ctx: Context inref) =
    let skipx = int ctx.Source.[0]
    let sizeCount = sbyte ctx.Source.[1]
    let ctx = Context.skip 2 skipx &ctx
    if sizeCount >= 0y then
        let size = int sizeCount
        Context.copy size &ctx
    else
        let count = int -sizeCount
        Context.fill count &ctx

let rec renderSSHPackets count (ctx: Context inref) =
    if count > 0 then
        let ctx' =  renderSSHPacket &ctx
        renderSSHPackets (count-1) &ctx'
    else
        ctx

let renderSSHLine width (ctx: Context inref) =
    let struct(skipy, pakets, offset) =
        let r = Context.readInt16 &ctx
        let v = r.Value
        if v &&& 0xC000s <> 0s then
            let r' = Context.readUInt16 &r.Ctx
            struct(int (-v), int r'.Value, 4)
        else
            struct(0, int v, 2)
    let dst = Context.skip offset (skipy * width) &ctx
    let next = renderSSHPackets pakets &dst 
    { Dest = ctx.Dest.Slice(width) 
      Source = next.Source }

let rec renderSSHLines width count (ctx: Context inref) =
    if count > 0 then
        let ctx' = renderSSHLine width &ctx
        renderSSHLines width (count-1) &ctx'
    else
        ctx

let renderSSH width (ctx: Context inref) =
    let r = Context.readUInt16 &ctx
    let lines = int r.Value
    renderSSHLines width lines &r.Ctx


let render (header: Header) initialView =
    fun (view: byte ViewMemory) (time: GameTime) (m: Memory<uint16>) ->
        let current =
            if view.IsEmpty then
                initialView
            else
                view

        let s = current.Span()
        let frameSize = (cast s : FrameHeader ReadOnlySpan).[0].Size
        let s = s.Slice(sizeof<FrameHeader>, frameSize - sizeof<FrameHeader>)
        let chunkType = (cast s).[0].Type
        let s = s.Slice(sizeof<ChunkHeader>)
        let ctx = { Source = s
                    Dest = m.Span }
        let __ =
            match chunkType with
            | ChunkType.Rle -> renderLines (int header.Height) &ctx
            | ChunkType.Delta -> renderSSH (int header.Width) &ctx
            | _ -> ctx

        current.Slice(frameSize)
            

type EventListener()  =
    inherit Diagnostics.Tracing.EventListener()
    
    [<Literal>]
    let GC_KEYWORD = 0x0000001UL
    [<Literal>]
    let STACK_KEYWORD = 0x40000000UL 
    [<Literal>]
    let TYPE_KEYWORD = 0x0080000UL
    
    [<Literal>]
    let GCHEAPANDTYPENAMES_KEYWORD = 0x1000000UL
    override this.OnEventSourceCreated(eventSource) =
        printfn "%O | %s" eventSource.Guid eventSource.Name

        // look for .NET Garbage Collection events
        if eventSource.Name = "Microsoft-Windows-DotNETRuntime" then
            this.EnableEvents(
                eventSource, 
                EventLevel.Verbose, 
                unbox (EventKeywords.ToObject(typedefof<EventKeywords>, STACK_KEYWORD ||| GC_KEYWORD ||| GCHEAPANDTYPENAMES_KEYWORD ||| TYPE_KEYWORD))
                )

    // from https://blogs.msdn.microsoft.com/dotnet/2018/12/04/announcing-net-core-2-2/
    // Called whenever an event is written.
    override this.OnEventWritten(eventData) =
        printfn "[%d] %O %s %s" eventData.OSThreadId eventData.EventId eventData.EventName eventData.Message
        
        for i in 0 .. eventData.Payload.Count - 1 do
            let payloadString = if isNull eventData.Payload.[i] then  String.Empty else string eventData.Payload.[i]
            printfn "   %s: %s"  eventData.PayloadNames.[i] payloadString
        
        
[<STAThread; EntryPoint>]
let Main args =
    let filename = 
        let path = Array.tryHead args
        match path with
        | Some p when File.Exists(p) ->
            p
        | _ ->
            let root = __SOURCE_DIRECTORY__
            //root + "/video/BBox.flh"
            root + "/video/Toaster.flh"
            
    use file = MemoryMappedFile.CreateFromFile(filename, FileMode.Open, null, 0L, MemoryMappedFiles.MemoryMappedFileAccess.Read)
    let accessor = file.CreateViewAccessor(0L,0L, MemoryMappedFileAccess.Read)
    let header : Header = accessor.Read(0L)
    let offset = accessor.Read<int>(80L)
    let offset2 = accessor.Read<int>(84L)
    let view1 = accessor.AsMemory<byte>(offset, int header.Size - offset)
    let view2 = accessor.AsMemory<byte>(offset2, int header.Size - offset2)
    let render = render header view2

    use win = new Window<uint16,_>(int header.Width, int header.Height, SurfaceFormat.Bgr565, view1, render, 30)
    //use listener = new EventListener()
    GC.Collect()
    win.Run()
    0


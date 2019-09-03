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



type WORD = uint16
type DWORD = uint32

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

[<Struct>]
type State = 
        { initialView: byte ViewMemory
          current: byte ViewMemory }
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
    let copy size ctx =
        let source = (MemoryMarshal.Cast ctx.Source).Slice(0,size)
        source.CopyTo(ctx.Dest)
        { Source = ctx.Source.Slice(size*2)
          Dest = ctx.Dest.Slice(size) }

    let fill size ctx =
        let color = uint16 (BinaryPrimitives.ReadInt16LittleEndian ctx.Source)
        let dest = ctx.Dest.Slice(0,size)
        dest.Fill(color)
        { Source = ctx.Source.Slice(2)
          Dest = ctx.Dest.Slice(size)}
        
    let readSByte ctx =
        RefTuple.make 
            (sbyte ctx.Source.[0])
            { ctx with Source = ctx.Source.Slice(1) }

    let readByte ctx =
        RefTuple.make
            (byte ctx.Source.[0])
            { ctx with Source = ctx.Source.Slice(1)  }

    let readUInt16 ctx =
        RefTuple.make
            (uint16 (BinaryPrimitives.ReadInt16LittleEndian ctx.Source))
            { ctx with Source = ctx.Source.Slice(2)  }

    let readInt16 ctx =
        RefTuple.make
            (BinaryPrimitives.ReadInt16LittleEndian ctx.Source)
            { ctx with Source = ctx.Source.Slice(2)  }

    let skip src dest ctx =
        { Source = ctx.Source.Slice(src)
          Dest = ctx.Dest.Slice(dest) }

let renderPacket ctx =
    let r =  Context.readSByte ctx
    let sizeCount = r.Value
    if sizeCount < 0y then
        let size = int (-sizeCount)
        Context.copy size r.Ctx
    else
        let count = int sizeCount
        Context.fill count r.Ctx

let rec renderPackets packets ctx =
    if packets > 0 then
        renderPackets (packets-1) (renderPacket ctx)
    else
        ctx

let renderLine ctx =
   let r = Context.readByte ctx
   let packets = int r.Value
   renderPackets packets r.Ctx

let rec renderLines height ctx =
    if height > 0 then
        renderLines (height-1) (renderLine ctx)
    else
        ctx

let renderBRUN height ctx =
    renderLines height ctx
   

let renderSSHPacket ctx =
    let r = Context.readByte ctx
    let skipx = int r.Value
    let r = Context.readSByte r.Ctx
    let sizeCount = r.Value
    let ctx = Context.skip 0 skipx r.Ctx
    if sizeCount >= 0y then
        let size = int sizeCount
        Context.copy size ctx
    else
        let count = int -sizeCount
        Context.fill count ctx

let rec renderSSHPackets count ctx =
    if count > 0 then
        renderSSHPackets (count-1) (renderSSHPacket ctx)
    else
        ctx

let renderSSHLine width ctx =
    let struct(skipy, pakets, offset) =
        let r = Context.readInt16 ctx
        let v = r.Value
        if v &&& 0xC000s <> 0s then
            let r' = Context.readUInt16 r.Ctx
            struct(int (-v), int r'.Value, 4)
        else
            struct(0, int v, 2)
    let dst = Context.skip offset (skipy * width) ctx
    let next = renderSSHPackets pakets dst 
    { Context.skip 0 width dst with Source = next.Source }

let rec renderSSHLines width count ctx =
    if count > 0 then
        renderSSHLines width (count-1) (renderSSHLine width ctx)
    else
        ctx

let renderSSH width ctx =
    let r = Context.readUInt16 ctx
    let lines = int r.Value
    renderSSHLines width lines r.Ctx

let render (header: Header) =
    fun (state: State) (time: GameTime) (m: Memory<uint16>) ->
        let state =
            if state.current.IsEmpty then
                { state with current = state.initialView }
            else
                state

        let s = state.current.Span()
        let frameHeader = MemoryMarshal.Cast<_,FrameHeader>(s)
        let frameSize = frameHeader.[0].Size
        let s = s.Slice(sizeof<FrameHeader>, frameSize - sizeof<FrameHeader>)
        let chunk = MemoryMarshal.Cast<_,ChunkHeader>(s)
        let s = s.Slice(sizeof<ChunkHeader>)
        let ctx = { Source = s
                    Dest = m.Span }
        let __ =
            match chunk.[0].Type with
            | ChunkType.Rle -> renderBRUN (int header.Height) ctx
            | ChunkType.Delta -> renderSSH (int header.Width) ctx
            | _ -> ctx

        { state with 
            current = state.current.Slice(frameSize) }
            

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
            root + "/video/toaster.flh"
            
    use file = MemoryMappedFile.CreateFromFile(filename, FileMode.Open, null, 0L, MemoryMappedFiles.MemoryMappedFileAccess.Read)
    let accessor = file.CreateViewAccessor(0L,0L, MemoryMappedFileAccess.Read)
    let header : Header = accessor.Read(0L)
    let offset = accessor.Read<int>(80L)
    let offset2 = accessor.Read<int>(84L)
    let view1 = accessor.AsMemory<byte>(offset, int header.Size - offset)
    let view2 = accessor.AsMemory<byte>(offset2, int header.Size - offset2)
    let state = { initialView = view2
                  current = view1 }

    let render = render header
   
    #if INTERACTIVE
    let buffer = Array.zeroCreate<uint16> (int header.Width * int header.Height) 
    let m = buffer.AsMemory()
    #endif


    use win = new Window<uint16,_>(int header.Width, int header.Height, SurfaceFormat.Bgr565, state, render, 30)
//    use listener = new EventListener()
    GC.Collect()
    win.Run()
    0


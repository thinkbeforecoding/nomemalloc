open System






//    _  _                                              _ _            
//  _| || |_                                           | | |           
// |_  __  _|_ __   ___  _ __ ___   ___ _ __ ___   __ _| | | ___   ___ 
//  _| || |_| '_ \ / _ \| '_ ` _ \ / _ \ '_ ` _ \ / _` | | |/ _ \ / __|
// |_  __  _| | | | (_) | | | | | |  __/ | | | | | (_| | | | (_) | (__ 
//   |_||_| |_| |_|\___/|_| |_| |_|\___|_| |_| |_|\__,_|_|_|\___/ \___|


let speaker = "Jérémie Chassaing"
let company = "D-EDGE"


let blog = Uri "https://thinkbeforecoding.com"
let twitter = @"thinkb4coding"





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
open System.Buffers

#nowarn "9"

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

type ViewMemoryManager<'t when 't: unmanaged>(accessor : MemoryMappedViewAccessor) =
    inherit MemoryManager<'t>()


    override this.Dispose(disposing) =
        if disposing then
            accessor.Dispose()

    override this.GetSpan() = 
        new Span<'t>(NativePtr.toVoidPtr<'t> (NativePtr.ofNativeInt<'t> (accessor.SafeMemoryMappedViewHandle.DangerousGetHandle())), int accessor.Capacity)


    override this.Pin(index) =
        let basePtr = NativePtr.ofNativeInt<'t> (accessor.SafeMemoryMappedViewHandle.DangerousGetHandle())
        let ptr = NativePtr.add basePtr index

        new MemoryHandle(NativePtr.toVoidPtr<'t> (ptr))
        

    override this.Unpin() = ()

                        
type MemoryMappedViewAccessor with
        member this.MemoryOwner<'t when 't: unmanaged>() = new ViewMemoryManager<'t>(this) :> IMemoryOwner<'t>
            
type Pixel = uint16
type Source = byte ReadOnlySpan
type Screen =  Pixel Span


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
    { Source: Source
      Screen: Screen }
    
module Immutable = 
    
    
    [<Struct; IsByRefLike>]
    type RefTuple<'l> =
        { Value: 'l
          Ctx: Context }
    
    module RefTuple =
        let inline make l ctx = { Value = l; Ctx = ctx}
    
    
    module Context =
        let copy size (ctx: Context inref) =
            let source =MemoryMarshal.Cast(ctx.Source).Slice(0,size)
            source.CopyTo(ctx.Screen)
            { Source = ctx.Source.Slice(size*sizeof<Pixel>)
              Screen = ctx.Screen.Slice(size) }
    
        let fill size (ctx: Context inref) =
            let color = uint16 (BinaryPrimitives.ReadInt16LittleEndian ctx.Source)
            let dest = ctx.Screen.Slice(0,size)
            dest.Fill(color)
            { Source = ctx.Source.Slice(2)
              Screen = ctx.Screen.Slice(size)}
            
        let inline readSByte (ctx: Context inref) =
            RefTuple.make 
                (sbyte ctx.Source[0])
                { ctx with Source = ctx.Source.Slice(1) }
    
        let inline readByte (ctx: Context inref) =
            RefTuple.make
                (byte ctx.Source[0])
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
              Screen = ctx.Screen.Slice(dest) }
    

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



    let renderSSHPacket (ctx: Context inref) =
        let skipx = int ctx.Source[0]
        let sizeCount = sbyte ctx.Source[1]
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
        let struct(skipy, packets, offset) =
            let r = Context.readInt16 &ctx
            let v = r.Value
            if v &&& 0xC000s <> 0s then
                let r' = Context.readUInt16 &r.Ctx
                struct(int (-v), int r'.Value, 4)
            else
                struct(0, int v, 2)
        let dst = Context.skip offset (skipy * width) &ctx
        let next = renderSSHPackets packets &dst 
        { Screen = ctx.Screen.Slice(width) 
          Source = next.Source }

    let rec renderSSHLines width count (ctx: Context inref) =
        if count > 0 then
            let ctx' = renderSSHLine width &ctx
            renderSSHLines width (count-1) &ctx'

    let renderSSH width (ctx: Context inref) =
        let r = Context.readUInt16 &ctx
        let lines = int r.Value
        renderSSHLines width lines &r.Ctx

    let renderFrame width height (source: ReadOnlySpan<byte>) (screen: Span<Pixel>) =
        let frameSize = MemoryMarshal.AsRef<FrameHeader>(source).Size
        let afterFrameHeader = source.Slice(sizeof<FrameHeader>, frameSize - sizeof<FrameHeader>)
        let chunkType = MemoryMarshal.AsRef<ChunkHeader>(afterFrameHeader).Type
        let bytes = afterFrameHeader.Slice(sizeof<ChunkHeader>)
        let ctx = { Source = bytes
                    Screen = screen }
        match chunkType with
        | ChunkType.Rle -> renderLines height &ctx
        | ChunkType.Delta -> renderSSH width &ctx 
        | _ -> ()

        frameSize

    let render (header: Header) initialView =
        fun (view: ReadOnlyMemory<byte>) (time: GameTime) (m: Memory<Pixel>) ->
            let current =
                if view.IsEmpty then
                    initialView
                else
                    view

            let frameSize = renderFrame (int header.Width) (int header.Height) current.Span m.Span

            current.Slice(frameSize)



module TwoIndices = 

    let inline copy (ctx: Context inref) size  (isrc: int) (iscreen: int) =
        let srcSlice = ctx.Source.Slice(isrc, size * sizeof<Pixel>)
        let screenSlice = ctx.Screen.Slice(iscreen, size)
        (MemoryMarshal.Cast srcSlice).CopyTo(screenSlice)
        struct (isrc + srcSlice.Length, iscreen + size)

    let inline fill (ctx: Context inref) count (isrc: int) (iscreen: int) =
       let color = uint16 (BinaryPrimitives.ReadInt16LittleEndian (ctx.Source.Slice(isrc)))
       let dest = ctx.Screen.Slice(iscreen,count)
       dest.Fill(color)
       struct (isrc + sizeof<Pixel>, iscreen + count)
 
    let renderPacket (ctx: Context inref) (isrc: int) (iscreen : int) =
        let sizeCount = sbyte ctx.Source[isrc]
        if sizeCount < 0y then
            let size = int (-sizeCount)
            copy &ctx size (isrc+1) iscreen
        else
            let count = int sizeCount
            fill &ctx count (isrc+1) iscreen


    let rec renderPackets (ctx: Context inref) packets (isrc: int) (iscreen : int)  =
        if packets > 0 then
            let struct (isrc', iscreen') = renderPacket &ctx isrc iscreen
            renderPackets &ctx (packets-1) isrc' iscreen'
        else
            struct(isrc, iscreen)

    let renderLine (ctx: Context inref) (isrc: int) (iscreen : int) =
       let packets = int ctx.Source[isrc]
       renderPackets &ctx packets (isrc+1) iscreen

    let rec renderLines height (ctx: Context inref) (isrc: int)  (iscreen : int) =
        if height > 0 then
            let struct(isrc', iscreen') = renderLine &ctx isrc iscreen
            renderLines (height-1) &ctx isrc' iscreen'



    let renderSSHPacket (ctx: Context inref) (isrc: int) (iscreen : int) =
        let skipx = int ctx.Source[isrc]
        let sizeCount = sbyte ctx.Source[isrc+1]
        let isrc' = isrc+2
        let iscreen' = iscreen + skipx
        if sizeCount >= 0y then
            let size = int sizeCount
            copy &ctx size isrc' iscreen'
        else
            let count = int -sizeCount
            fill &ctx count isrc' iscreen'

    let rec renderSSHPackets (ctx: Context inref) count  (isrc: int)  (iscreen : int) =
        if count > 0 then
            let struct(isrc', iscreen') =  renderSSHPacket &ctx isrc iscreen
            renderSSHPackets &ctx (count-1) isrc'  iscreen'
        else
            isrc

    let renderSSHLine (ctx: Context inref) width (isrc: int) (iscreen : int) =
        let struct(iscreen', packets, isrc') =
            let v = BinaryPrimitives.ReadInt16LittleEndian (ctx.Source.Slice(isrc))

            if v &&& 0xC000s <> 0s then
                let packetCount = BinaryPrimitives.ReadUInt16LittleEndian (ctx.Source.Slice(isrc+sizeof<int16>))
                struct(iscreen + int (-v) * width, int packetCount, isrc + sizeof<int16> + sizeof<uint16>)
            else
                struct(iscreen, int v, isrc + sizeof<int16>)
        renderSSHPackets &ctx packets isrc' iscreen' 

    let rec renderSSHLines (ctx: Context inref) width count (isrc: int) (iscreen : int) =
        if count > 0 then
            let isrc' = renderSSHLine &ctx width isrc iscreen
            renderSSHLines &ctx width (count-1) isrc' (iscreen+width)

    let renderSSH width (ctx: Context inref) (isrc: int) (iscreen : int) =
        let lines = int (BinaryPrimitives.ReadUInt16LittleEndian (ctx.Source.Slice(isrc)))
        renderSSHLines &ctx width lines (isrc+sizeof<uint16>) iscreen


    let renderFrame width height (source: ReadOnlySpan<byte>) (screen: Span<Pixel>) =
        let frameSize = MemoryMarshal.AsRef<FrameHeader>(source).Size
        let afterFrameHeader = source.Slice(sizeof<FrameHeader>, frameSize - sizeof<FrameHeader>)
        let chunkType = MemoryMarshal.AsRef<ChunkHeader>(afterFrameHeader).Type
        let bytes = afterFrameHeader.Slice(sizeof<ChunkHeader>)
        let ctx = { Source = bytes
                    Screen = screen }
        match chunkType with
        | ChunkType.Rle -> renderLines height &ctx 0 0
        | ChunkType.Delta -> renderSSH width &ctx 0 0
        | _ -> ()

        frameSize


    let render (header: Header) initialView =
        fun (view: ReadOnlyMemory<byte>) (time: GameTime) (m: Memory<Pixel>) ->
            let current =
                if view.IsEmpty then
                    initialView
                else
                    view

            let frameSize = renderFrame (int header.Width) (int header.Height) current.Span m.Span

            current.Slice(frameSize)


module Compact =
    [<Struct>]
    type Index(value: int64) =
        member _.Source = int value 
        member _.Screen = int (value >>> 32)

        member _.Value = value

        member _.AddSource x = Index(value + int64 x)
        member _.AddScreen x = Index(value + ((int64 x) <<< 32))


        member _.Add(x,y) = Index(value + int64 x + (int64 y <<< 32))

        new(x,y) = Index(int64 x ||| (int64 y <<< 32))

        override this.ToString() =
            $"{this.Source};{this.Screen}"

        static member Zero = Index 0L



    let inline copy (ctx: Context inref)  (i: Index) size  =
        let srcSlice = ctx.Source.Slice(i.Source, size * sizeof<Pixel>)
        let screenSlice = ctx.Screen.Slice(i.Screen, size)
        (MemoryMarshal.Cast srcSlice).CopyTo(screenSlice)
        i.Add(size * sizeof<Pixel>, size)


    let inline fill (ctx: Context inref) (i: Index) count =
        let color = uint16 (BinaryPrimitives.ReadInt16LittleEndian (ctx.Source.Slice(i.Source)))
        let dest = ctx.Screen.Slice(i.Screen, count)
        dest.Fill(color)
        i.Add(sizeof<Pixel>, count)

    let inline renderPacket (ctx: Context inref) (i: Index)  =
        let sizeCount = sbyte ctx.Source[i.Source]
        let i' = i.AddSource 1
        if sizeCount < 0y then
            let size = int (-sizeCount)
            copy &ctx i' size
        else
            let count = int sizeCount
            fill &ctx i' count

    let rec renderPackets packets (ctx: Context inref) (i: Index)  =
        if packets > 0uy then
            let i' = renderPacket &ctx i
            renderPackets (packets-1uy) &ctx i'
        else
            i

    let inline renderLine (ctx: Context inref) (i: Index) =
        let packets = ctx.Source[i.Source]
        renderPackets packets &ctx ( i.AddSource 1)

    let rec renderLines height (ctx: Context inref) (i: Index) =
        if height > 0 then
            let i' = renderLine &ctx i
            renderLines (height-1) &ctx  i' 

    let inline renderSSHPacket (ctx: Context inref) (i: Index) =
        let skipx = int ctx.Source[i.Source]
        let sizeCount = sbyte ctx.Source[i.Source+1]
        let i' = i.Add(2, skipx)
        if sizeCount >= 0y then
            let size = int sizeCount
            copy &ctx i' size
        else
            let count = int -sizeCount
            fill &ctx i' count

    let rec renderSSHPackets (ctx: Context inref) (i: Index) count =
        if count > 0us then
            let i' = renderSSHPacket &ctx i 
            renderSSHPackets &ctx i' (count-1us)
        else
            i

    let inline renderSSHLine width (ctx: Context inref) (i: Index) =
        let v = BinaryPrimitives.ReadInt16LittleEndian (ctx.Source.Slice(i.Source))

        if v &&& 0xC000s <> 0s then
            let packets = BinaryPrimitives.ReadUInt16LittleEndian (ctx.Source.Slice(i.Source+sizeof<int16>))
            renderSSHPackets &ctx (i.Add(sizeof<int16> + sizeof<uint16>, int (-v) * width)) packets
        else
            let packets = uint16 v
            renderSSHPackets &ctx (i.AddSource sizeof<int16>) packets 

    let rec renderSSHLines count width (ctx: Context inref) (i: Index)   =
        if count > 0 then
            let i' = renderSSHLine width &ctx i
            renderSSHLines (count-1) width &ctx (Index(i'.Source, i.Screen+width))

    let inline renderSSH width (ctx: Context inref) (i: Index)  =
        let lines = int (BinaryPrimitives.ReadUInt16LittleEndian (ctx.Source.Slice(i.Source)))
        renderSSHLines lines width &ctx (i.AddSource sizeof<uint16>)

    let renderFrame width height (source: ReadOnlySpan<byte>) (screen: Span<Pixel>) =
        let frameSize = MemoryMarshal.AsRef<FrameHeader>(source).Size
        let afterFrameHeader = source.Slice(sizeof<FrameHeader>, frameSize - sizeof<FrameHeader>)
        let chunkType = MemoryMarshal.AsRef<ChunkHeader>(afterFrameHeader).Type
        let bytes = afterFrameHeader.Slice(sizeof<ChunkHeader>)
        let ctx = { Source = bytes
                    Screen = screen }
        match chunkType with
        | ChunkType.Rle -> renderLines height &ctx Index.Zero 
        | ChunkType.Delta -> renderSSH width &ctx Index.Zero 
        | _ -> ()

        frameSize

    let render (header: Header) initialView =
        fun (view: ReadOnlyMemory<byte>) (time: GameTime) (m: Memory<Pixel>) ->
            let current =
                if view.IsEmpty then
                    initialView
                else
                    view

            let frameSize = renderFrame (int header.Width) (int header.Height) current.Span m.Span

            current.Slice(frameSize)

module CompactMutable =
    [<Struct>]
    type Index(value: int64) =
        member _.Source = int value 
        member _.Screen = int (value >>> 32)

        member _.Value = value

        member _.AddSource x = Index(value + int64 x)
        member _.AddScreen x = Index(value + ((int64 x) <<< 32))

        member _.Add(x,y) = Index(value + (int64 x ||| (int64 y <<< 32) ))

        new(x,y) = Index(int64 x ||| (int64 y <<< 32))

        override this.ToString() =
            $"{this.Source};{this.Screen}"

        static member Zero = Index 0L



    let inline copy (ctx: Context inref)  (i: Index) size  =
        let srcSlice = ctx.Source.Slice(i.Source, size * sizeof<Pixel>)
        let screenSlice = ctx.Screen.Slice(i.Screen, size)
        (MemoryMarshal.Cast srcSlice).CopyTo(screenSlice)
        i.Add(size * sizeof<Pixel>, size)


    let inline fill (ctx: Context inref) (i: Index) count =
        let color = uint16 (BinaryPrimitives.ReadInt16LittleEndian (ctx.Source.Slice(i.Source)))
        let dest = ctx.Screen.Slice(i.Screen, count)
        dest.Fill(color)
        i.Add(sizeof<Pixel>, count)

    let inline renderPacket (ctx: Context inref) (i: Index)  =
        let sizeCount = sbyte ctx.Source[i.Source]
        let i' = i.AddSource 1
        if sizeCount < 0y then
            let size = int (-sizeCount)
            copy &ctx i' size
        else
            let count = int sizeCount
            fill &ctx i' count

    let renderPackets packets (ctx: Context inref) (i: Index)  =
        let mutable i = i
        for _ in 0 .. packets-1 do
            i <- renderPacket &ctx i
        i

    let inline renderLine (ctx: Context inref) (i: Index) =
        let packets = int ctx.Source[i.Source]
        renderPackets packets &ctx ( i.AddSource 1)

    let renderLines height (ctx: Context inref) (i: Index) =
        let mutable i = i
        for _ in 0 .. height-1 do
            i <- renderLine &ctx i


    let inline renderSSHPacket (ctx: Context inref) (i: Index) =
        let skipx = int ctx.Source[i.Source]
        let sizeCount = sbyte ctx.Source[i.Source+1]
        let i' = i.Add(2, skipx)
        if sizeCount >= 0y then
            let size = int sizeCount
            copy &ctx i' size
        else
            let count = int -sizeCount
            fill &ctx i' count

    let renderSSHPackets (ctx: Context inref) (i: Index) count =
        let mutable i = i
        for _ in 0 .. count-1 do
            i <- renderSSHPacket &ctx i 
        i

    let inline renderSSHLine width (ctx: Context inref) (i: Index) =
        let v = BinaryPrimitives.ReadInt16LittleEndian (ctx.Source.Slice(i.Source))

        if v &&& 0xC000s <> 0s then
            let packetCount = BinaryPrimitives.ReadUInt16LittleEndian (ctx.Source.Slice(i.Source+sizeof<int16>))
            let packets = int packetCount
            renderSSHPackets &ctx (i.Add(sizeof<int16> + sizeof<uint16>, int (-v) * width)) packets
        else
            let packets = int v
            renderSSHPackets &ctx (i.AddSource sizeof<int16>) packets 

    let renderSSHLines count width (ctx: Context inref) (i: Index)   =
        let mutable i = i
        for _ in 0 .. count-1 do
        if count > 0 then
            let i' =  renderSSHLine width &ctx i
            i <- Index(i'.Source, i.Screen+width)

    let inline renderSSH width (ctx: Context inref) (i: Index)  =
        let lines = int (BinaryPrimitives.ReadUInt16LittleEndian (ctx.Source.Slice(i.Source)))
        renderSSHLines lines width &ctx (i.AddSource sizeof<uint16>)

    let renderFrame width height (source: ReadOnlySpan<byte>) (screen: Span<Pixel>) =
        let frameSize = MemoryMarshal.AsRef<FrameHeader>(source).Size
        let afterFrameHeader = source.Slice(sizeof<FrameHeader>, frameSize - sizeof<FrameHeader>)
        let chunkType = MemoryMarshal.AsRef<ChunkHeader>(afterFrameHeader).Type
        let bytes = afterFrameHeader.Slice(sizeof<ChunkHeader>)
        let ctx = { Source = bytes
                    Screen = screen }
        match chunkType with
        | ChunkType.Rle -> renderLines height &ctx Index.Zero 
        | ChunkType.Delta -> renderSSH width &ctx Index.Zero 
        | _ -> ()

        frameSize

    let render (header: Header) initialView =
        fun (view: ReadOnlyMemory<byte>) (time: GameTime) (m: Memory<Pixel>) ->
            let current =
                if view.IsEmpty then
                    initialView
                else
                    view

            let frameSize = renderFrame (int header.Width) (int header.Height) current.Span m.Span

            current.Slice(frameSize)




module Mutable =

    let inline copy (ctx: Context inref)  (isrc: int byref) (iscreen: int byref) size  =
        let srcSlice = ctx.Source.Slice(isrc, size * sizeof<Pixel>)
        let screenSlice = ctx.Screen.Slice(iscreen, size)
        (MemoryMarshal.Cast srcSlice).CopyTo(screenSlice)
        isrc <- isrc + size * sizeof<Pixel>
        iscreen <- iscreen + size


    let inline fill (ctx: Context inref) (isrc: int byref) (iscreen: int byref) count =
        let color = uint16 (BinaryPrimitives.ReadInt16LittleEndian (ctx.Source.Slice(isrc)))
        let dest = ctx.Screen.Slice(iscreen, count)
        dest.Fill(color)
        isrc <- isrc + sizeof<Pixel>
        iscreen <- iscreen + count

    let renderPackets packets (ctx: Context inref) (isrc: int byref) (iscreen: int byref)  =
        for i in 0 .. packets-1 do
            let sizeCount = sbyte ctx.Source[isrc]
            isrc <- isrc+1
            if sizeCount < 0y then
                let size = int (-sizeCount)
                copy &ctx &isrc &iscreen size
            else
                let count = int sizeCount
                fill &ctx &isrc &iscreen count


    let renderLines height (ctx: Context inref) (isrc: int byref) (iscreen: int byref) =
        for i in 0 .. height-1 do
            let packets = int ctx.Source[isrc]
            isrc <- isrc+1
            renderPackets packets &ctx &isrc &iscreen


    let renderSSHPackets (ctx: Context inref) (isrc: int byref) (iscreen: int byref) count =
        for i in 0 .. count-1 do
            let skipx = int ctx.Source[isrc]
            let sizeCount = sbyte ctx.Source[isrc+1]
            isrc <- isrc + 2
            iscreen <- iscreen + skipx
            if sizeCount >= 0y then
                let size = int sizeCount
                copy &ctx &isrc &iscreen size
            else
                let count = int -sizeCount
                fill &ctx &isrc &iscreen count


    let inline renderSSHLine width (ctx: Context inref) (isrc: int byref) (iscreen: int byref) =
        let v = BinaryPrimitives.ReadInt16LittleEndian (ctx.Source.Slice(isrc))

        if v &&& 0xC000s <> 0s then
            let packetCount = BinaryPrimitives.ReadUInt16LittleEndian (ctx.Source.Slice(isrc+sizeof<int16>))
            let packets = int packetCount
            isrc <- isrc + sizeof<int16> + sizeof<uint16>
            iscreen <- iscreen + int (-v) * width
            renderSSHPackets &ctx &isrc &iscreen packets
        else
            let packets = int v
            isrc <- isrc + sizeof<int16>
            renderSSHPackets &ctx &isrc &iscreen packets 

    let renderSSHLines count width (ctx: Context inref) (isrc: int byref) (iscreen: int byref) =
        for i in 0 .. count-1 do
            let nextiscreen = iscreen + width
            renderSSHLine width &ctx &isrc &iscreen
            iscreen <- nextiscreen

    let inline renderSSH width (ctx: Context inref) (isrc: int byref) (iscreen: int byref)  =
        let lines = int (BinaryPrimitives.ReadUInt16LittleEndian (ctx.Source.Slice(isrc)))
        isrc <- isrc + sizeof<uint16>
        renderSSHLines lines width &ctx &isrc &iscreen

    let renderFrame width height (source: ReadOnlySpan<byte>) (screen: Span<Pixel>) =
        let frameSize = MemoryMarshal.AsRef<FrameHeader>(source).Size
        let afterFrameHeader = source.Slice(sizeof<FrameHeader>, frameSize - sizeof<FrameHeader>)
        let chunkType = MemoryMarshal.AsRef<ChunkHeader>(afterFrameHeader).Type
        let bytes = afterFrameHeader.Slice(sizeof<ChunkHeader>)
        let ctx = { Source = bytes
                    Screen = screen }
        let mutable isrc = 0
        let mutable iscreen = 0

        match chunkType with
        | ChunkType.Rle -> renderLines height &ctx &isrc &iscreen
        | ChunkType.Delta -> renderSSH width &ctx &isrc &iscreen
        | _ -> ()

        frameSize

    let render (header: Header) initialView =
        fun (view: ReadOnlyMemory<byte>) (time: GameTime) (m: Memory<Pixel>) ->
            let current =
                if view.IsEmpty then
                    initialView
                else
                    view

            let frameSize = renderFrame (int header.Width) (int header.Height) current.Span m.Span

            current.Slice(frameSize)

let benchRender runs summary render =
    let mutable totalTime = 0L
    let mutable count = 0
    

    fun view time m ->
        let sw = Diagnostics.Stopwatch.StartNew()
        try
            for i in 1 .. runs-1 do
                render view time m |> ignore
            render view time m
        finally
            totalTime <- totalTime + sw.ElapsedMilliseconds
            count <- count + runs 
            if count >= summary then
                printfn $"%2f{float totalTime / float count}"
                totalTime <- 0L
                count <- 0


            

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
            let payloadString = if isNull eventData.Payload[i] then  String.Empty else string eventData.Payload[i]
            printfn "   %s: %s"  eventData.PayloadNames[i] payloadString
        
        
[<STAThread; EntryPoint>]
let Main args =
    let filename = 
        let path = Array.tryHead args
        match path with
        | Some p when File.Exists(p) ->
            p
        | _ ->
            let root = __SOURCE_DIRECTORY__
            root + "/video/BBox.flh"
            //root + "/video/Toaster.flh"
            
    use file = MemoryMappedFile.CreateFromFile(filename, FileMode.Open, null, 0L, MemoryMappedFiles.MemoryMappedFileAccess.Read)
    let accessor = file.CreateViewAccessor(0L,0L, MemoryMappedFileAccess.Read)
    use owner = accessor.MemoryOwner<byte>()
    let memory = owner.Memory
    let span  = Span.op_Implicit memory.Span 
    let header : Header =  MemoryMarshal.Read(span)
     
    let offset = BinaryPrimitives.ReadInt32LittleEndian(span.Slice(80))
    let offset2 = BinaryPrimitives.ReadInt32LittleEndian(span.Slice(84))
    
    let view1 = owner.Memory.Slice(offset, int header.Size - offset) |> Memory.op_Implicit

    let view2 = owner.Memory.Slice(offset2, int header.Size - offset2) |> Memory.op_Implicit
    //let render = benchRender 100 10000 (Immutable.render header view2)
    //let render = benchRender 100 10000 (TwoIndices.render header view2)
    //let render = benchRender 100 10000 (Compact.render header view2)
    //let render = Compact.render header view2
    //let render = benchRender 100 10000 (CompactMutable.render header view2)
    let render = benchRender 100 10000 (Mutable.render header view2)
    //let render = Mutable.render header view2

    use win = new Window<Pixel,_>(int header.Width, int header.Height, SurfaceFormat.Bgr565, view1, render, 30)
    use listener = new EventListener()
    GC.Collect()
    win.Run()
    0


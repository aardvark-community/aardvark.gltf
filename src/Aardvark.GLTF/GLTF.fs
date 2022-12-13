namespace Aardvark.GLTF

open System.Runtime.InteropServices
open glTFLoader
open glTFLoader.Schema
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.Rendering
open Microsoft.FSharp.NativeInterop
open Aardvark.GLTF

#nowarn "9"


module GLTF =
    
    [<AutoOpen>]
    module private Utilities =
        open System.IO
        
        type System.IO.Stream with
            member x.ReadToEnd() =
                let remSize = 
                    try
                        let len = x.Length
                        let pos = x.Position
                        int (len - pos)
                    with _ ->
                        1 <<< 20
                        
                let mutable arr = Array.zeroCreate<byte> remSize
                
                let mutable o = 0
                let mutable rem = arr.Length
                let mutable finished = false
                while not finished do
                    if rem < o then
                        System.Array.Resize(&arr, arr.Length <<< 1)
                        rem <- arr.Length - o
                        
                    let r = x.Read(arr, o, rem)
                    if r = 0 then
                        finished <- true
                    else
                        rem <- rem - r
                        o <- o + r
                        
                if o < arr.Length then
                    System.Array.Resize(&arr, o)
                arr
        
        type UnclosableStream(inner : Stream) =
            inherit Stream()
            
            override this.CanRead = inner.CanRead
            override this.CanSeek = inner.CanSeek
            override this.CanWrite = inner.CanWrite
            override this.Length = inner.Length
            override this.Position with get() = inner.Position and set(v) = inner.Position <- v
            override this.Flush() = inner.Flush()
            override this.Seek(offset, origin) = inner.Seek(offset, origin)
            override this.SetLength(value) = inner.SetLength(value)
            override this.Read(buffer, offset, count) = inner.Read(buffer, offset, count)
            override this.Write(buffer, offset, count) = inner.Write(buffer, offset, count)
            override this.Dispose(disposing : bool) = ()
            override this.Close() = ()
               
        let getArray<'a when 'a : unmanaged> (cache : Dict<_,System.Array>) (byteOffset : int) (byteStride : int) (cnt : int) (buffer : byte[]) =      
            cache.GetOrCreate((buffer, byteOffset, byteStride, cnt, typeof<'a>), fun (buffer, byteOffset, byteStride, cnt, _) ->
                let size = sizeof<'a>
                if byteStride = 0 || byteStride = size then
                    let arr = Array.zeroCreate<'a> cnt
                    use ptr = fixed arr
                    let src = System.Span(buffer, byteOffset, buffer.Length - byteOffset)
                    let dst = System.Span<byte>(NativePtr.toVoidPtr ptr, arr.Length * size)
                    src.Slice(0, arr.Length * size).CopyTo(dst)
                    arr
                else
                    let res = Array.zeroCreate<'a> cnt
                    use pSrc = fixed buffer
                    use pDst = fixed res
                    
                    let mutable pSrc = NativePtr.ofNativeInt<'a> (NativePtr.toNativeInt pSrc + nativeint byteOffset)
                    let mutable pDst = pDst
                    for i in 0 .. cnt - 1 do
                        NativePtr.write pDst (NativePtr.read pSrc)
                        pSrc <- NativePtr.ofNativeInt (NativePtr.toNativeInt pSrc + nativeint byteStride)
                        pDst <- NativePtr.add pDst 1
                    res
            ) :?> 'a[]
            
        let getAttributeArray (cache : Dict<_,System.Array>) (readBuffer : int -> byte[]) (model : Gltf) (accId : int) =
            let acc = model.Accessors.[accId]
            if acc.BufferView.HasValue then
                let view = model.BufferViews.[acc.BufferView.Value]
                let bufferData = readBuffer view.Buffer
                let stride = if view.ByteStride.HasValue then view.ByteStride.Value else 0
                
                let byteOffset = view.ByteOffset + acc.ByteOffset
                
                match acc.ComponentType with
                | Accessor.ComponentTypeEnum.UNSIGNED_BYTE ->
                    match acc.Type with
                    | Accessor.TypeEnum.SCALAR -> getArray<byte> cache byteOffset stride acc.Count bufferData :> System.Array
                    | Accessor.TypeEnum.VEC3 -> getArray<C3b> cache byteOffset stride acc.Count bufferData :> System.Array
                    | Accessor.TypeEnum.VEC4 -> getArray<C4b> cache byteOffset stride acc.Count bufferData :> System.Array
                    | _ -> failwith ""
                | Accessor.ComponentTypeEnum.UNSIGNED_SHORT ->
                    match acc.Type with
                    | Accessor.TypeEnum.SCALAR -> getArray<uint16> cache byteOffset stride acc.Count bufferData :> System.Array
                    | Accessor.TypeEnum.VEC3 -> getArray<C3us> cache byteOffset stride acc.Count bufferData :> System.Array
                    | Accessor.TypeEnum.VEC4 -> getArray<C4us> cache byteOffset stride acc.Count bufferData :> System.Array
                    | _ -> failwith ""
                | Accessor.ComponentTypeEnum.UNSIGNED_INT->
                    match acc.Type with
                    | Accessor.TypeEnum.SCALAR -> getArray<uint32> cache byteOffset stride acc.Count bufferData :> System.Array
                    | Accessor.TypeEnum.VEC2 -> getArray<V2ui> cache byteOffset stride acc.Count bufferData :> System.Array
                    | Accessor.TypeEnum.VEC3 -> getArray<V3ui> cache byteOffset stride acc.Count bufferData :> System.Array
                    | Accessor.TypeEnum.VEC4 -> getArray<V4ui> cache byteOffset stride acc.Count bufferData :> System.Array
                    | _ -> failwith ""
                | Accessor.ComponentTypeEnum.FLOAT ->
                    match acc.Type with
                    | Accessor.TypeEnum.SCALAR -> getArray<float32> cache byteOffset stride acc.Count bufferData :> System.Array
                    | Accessor.TypeEnum.VEC2 -> getArray<V2f> cache byteOffset stride acc.Count bufferData :> System.Array
                    | Accessor.TypeEnum.VEC3 -> getArray<V3f> cache byteOffset stride acc.Count bufferData :> System.Array
                    | Accessor.TypeEnum.VEC4 -> getArray<V4f> cache byteOffset stride acc.Count bufferData :> System.Array
                    | _ -> failwith ""
                | Accessor.ComponentTypeEnum.SHORT ->
                    match acc.Type with
                    | Accessor.TypeEnum.SCALAR -> getArray<int16> cache byteOffset stride acc.Count bufferData :> System.Array
                    | _ -> failwith ""
                | Accessor.ComponentTypeEnum.BYTE ->
                    match acc.Type with
                    | Accessor.TypeEnum.SCALAR -> getArray<int8> cache byteOffset stride acc.Count bufferData :> System.Array
                    | _ -> failwith ""
                | _ ->
                    failwith ""
            else
                failwith ""
        
        let flipY (arr : System.Array) =
            match arr with
            | :? array<V2f> as arr -> arr |> Array.map (fun v -> V2f(v.X, 1.0f - v.Y)) :> System.Array
            | :? array<V2d> as arr -> arr |> Array.map (fun v -> V2d(v.X, 1.0 - v.Y)) :> System.Array
            | _ -> failwithf "bad TC type: %A" arr
        
        let getTrafo (node : glTFLoader.Schema.Node) =

            let mutable trafo = Trafo3d.Identity
            let mutable changed = false
            
            if not (isNull node.Matrix) then
                changed <- true
                let m = node.Matrix
                let fw =
                    M44d(
                        float m.[0], float m.[4], float m.[8], float m.[12],
                        float m.[1], float m.[5], float m.[9], float m.[13],
                        float m.[2], float m.[6], float m.[10], float m.[14],
                        float m.[3], float m.[7], float m.[11], float m.[15]
                    )
                trafo <- trafo * Trafo3d(fw, fw.Inverse)
                
            if not (isNull node.Scale) then
                changed <- true
                let s = node.Scale
                trafo <- trafo * Trafo3d.Scale(float s.[0], float s.[1], float s.[2])
            
            if not (isNull node.Rotation) then
                changed <- true
                let q = node.Rotation
                let q = QuaternionD(float q.[3], float q.[0], float q.[1], float q.[2])
                let q = Rot3d(q.Normalized)
                
                let q : Trafo3d = q |> Rot3d.op_Explicit
                trafo <- trafo * q
                
            if not (isNull node.Translation) then
                changed <- true
                let t = node.Translation
                trafo <- trafo * Trafo3d.Translation(V3d(float t.[0], float t.[1], float t.[2]))
                
            if not changed || trafo.Forward.IsIdentity() then
                None
            else
                Some trafo
                
        let toScene (read : string -> byte[]) (model : Gltf) =
            
            let bufferCache = Dict<int, byte[]>()
            
            let readBuffer (i : int) =
                bufferCache.GetOrCreate(i, fun i ->
                    model.LoadBinaryBuffer(i, read)    
                )
            
            
            let mutable imageSemantics = HashMap.empty<int, HashSet<TextureSemantic>>
            let inline addTexture (sems : list<TextureSemantic>) (info : TextureInfo) =
                if not (isNull info) then
                    let imageId = model.Textures.[info.Index].Source
                    if imageId.HasValue then
                        let id = imageId.Value
                        imageSemantics <-
                            imageSemantics |> HashMap.alter id (function
                                | Some o -> Some ((o, sems) ||> List.fold (fun o sem -> HashSet.add sem o))
                                | None -> Some (HashSet.ofList sems)
                            )
                            
            if not (isNull model.Materials) then
                for mat in model.Materials do
                    addTexture [TextureSemantic.Emissive] mat.EmissiveTexture
                    if not (isNull mat.NormalTexture) then
                        addTexture [TextureSemantic.Normal] (TextureInfo(Index = mat.NormalTexture.Index))
                    if not (isNull mat.PbrMetallicRoughness) then
                        addTexture [TextureSemantic.BaseColor] mat.PbrMetallicRoughness.BaseColorTexture
                        addTexture [TextureSemantic.Metallicness; TextureSemantic.Roughness] mat.PbrMetallicRoughness.MetallicRoughnessTexture
                        
            let images =
                if isNull model.Images || model.Images.Length = 0 then
                    [||]
                else
                    model.Images |> Array.mapi (fun ii img ->
                        if img.BufferView.HasValue then
                            let id = ImageId.New()
                            let view = model.BufferViews.[img.BufferView.Value]
                            let buffer = readBuffer view.Buffer
                            
                            let data = Array.sub buffer view.ByteOffset view.ByteLength
                            
                            let mime =
                                if img.MimeType.HasValue then
                                    match img.MimeType.Value with
                                    | Image.MimeTypeEnum.image_jpeg -> Some "image/jpeg"
                                    | Image.MimeTypeEnum.image_png -> Some "image/png"
                                    | _ -> None
                                else
                                    None
                                    
                            let sem =
                                match HashMap.tryFind ii imageSemantics with
                                | Some sem -> sem
                                | None -> HashSet.empty
                                    
                            let data =
                                {
                                    Name = if System.String.IsNullOrEmpty img.Name then None else Some img.Name
                                    MimeType = mime
                                    Data = data
                                    Semantics = sem
                                }
                            
                            Some (id, data)
                            // try
                            //     let pimg = 
                            //         use ms = new MemoryStream(buffer, view.ByteOffset, view.ByteLength)
                            //         PixImage.Load(ms)
                            //         
                            //     Some (id, pimg)
                            // with e ->
                            //     Log.warn "could not load image %A (%A)" img.Name img.Uri
                            //     None
                        else
                            None
                    )
            

            let materials =
                if isNull model.Materials then
                    [||]
                else
                    model.Materials |> Array.map (fun mat ->
                        let id = MaterialId.New()
                        let mutable tcMapping = HashMap.empty<int, HashSet<TextureSemantic>>
                        
                        let opaque = mat.AlphaMode <> Material.AlphaModeEnum.BLEND
                        let doubleSided = mat.DoubleSided
                        
                        let tryGetTextureWithIndex (sem : list<TextureSemantic>) (index : int) (tcIndex : int) =
                            let tex = model.Textures.[index]
                            if tex.Source.HasValue then
                                match images.[tex.Source.Value] with
                                | Some (imageId, _) ->
                                    tcMapping <-
                                        tcMapping
                                        |> HashMap.alter tcIndex (function
                                            | Some o -> Some ((o, sem) ||> List.fold (fun o sem -> HashSet.add sem o))
                                            | None -> Some (HashSet.ofList sem)
                                        )
                                    Some imageId
                                | None ->
                                    None
                            else
                                None
                    
                        let tryGetTexture (sem : list<TextureSemantic>) (info : TextureInfo) =
                            if isNull info then
                                None
                            else
                                tryGetTextureWithIndex sem info.Index info.TexCoord
                        
                        let albedoColor =
                            if isNull mat.PbrMetallicRoughness then C4f(1.0f, 1.0f, 1.0f, 1.0f)
                            else C4f mat.PbrMetallicRoughness.BaseColorFactor
                        
                        let albedoTexture =
                            if isNull mat.PbrMetallicRoughness then None
                            else tryGetTexture [TextureSemantic.BaseColor] mat.PbrMetallicRoughness.BaseColorTexture
                            
                        let roughness =
                            if isNull mat.PbrMetallicRoughness then 1.0
                            else float mat.PbrMetallicRoughness.RoughnessFactor
                            
                            
                        let roughnessTexture =
                            if isNull mat.PbrMetallicRoughness then None
                            else tryGetTexture [TextureSemantic.Roughness; TextureSemantic.Metallicness] mat.PbrMetallicRoughness.MetallicRoughnessTexture
                            
                        let metallicness =
                            if isNull mat.PbrMetallicRoughness then 0.0
                            else float mat.PbrMetallicRoughness.MetallicFactor
                            
                        let emissive =
                            if isNull mat.EmissiveFactor then C4f(0.0f, 0.0f, 0.0f, 0.0f)
                            else C3f(mat.EmissiveFactor).ToC4f()
                            
                        let emissiveTexture =
                            tryGetTexture [TextureSemantic.Emissive] mat.EmissiveTexture
                            
                        let normalTexture =
                            if isNull mat.NormalTexture then None
                            else tryGetTextureWithIndex [TextureSemantic.Normal] mat.NormalTexture.Index mat.NormalTexture.TexCoord
                            
                        let normalTextureScale =
                            if isNull mat.NormalTexture then 1.0
                            else float mat.NormalTexture.Scale
                            
                        id, {
                            Name                = if System.String.IsNullOrEmpty mat.Name then None else Some mat.Name
                                
                            DoubleSided         = doubleSided
                            Opaque              = opaque
                                
                            BaseColorTexture       = albedoTexture
                            BaseColor         = albedoColor
                            Roughness           = roughness
                            RoughnessTexture    = roughnessTexture
                            RoughnessTextureComponent  = 1
                            
                            Metallicness        = metallicness
                            MetallicnessTexture = roughnessTexture
                            MetallicnessTextureComponent  = 2
                            
                            EmissiveColor       = emissive
                            EmissiveTexture     = emissiveTexture
                            
                            NormalTexture       = normalTexture
                            NormalTextureScale  = normalTextureScale
                        }, tcMapping
                    )
        
            let arrayCache = Dict()
            let geometryIds = Dict()
            let meshes =
                if isNull model.Meshes then
                    [||]
                else
                    model.Meshes |> Array.map (fun m ->
                    
                        m.Primitives |> Array.choose (fun p ->
                            let index =
                                if p.Indices.HasValue then
                                    getAttributeArray arrayCache readBuffer model p.Indices.Value
                                else
                                    null
                                    
                            let attributes =
                                p.Attributes |> Seq.toArray |> Array.map (fun (KeyValue(name, att)) ->
                                    let arr = getAttributeArray arrayCache readBuffer model att
                                    let arr =
                                        if name.StartsWith "TEXCOORD" then flipY arr
                                        else arr
                                    
                                    name, arr
                                )
                                
                            let attributeMap =
                                HashMap.ofArray attributes
                              
                              
                              
                            match HashMap.tryFind "POSITION" attributeMap with
                            | Some (:? array<V3f> as position) ->  
                                let bounds =
                                    let acc = model.Accessors.[p.Attributes.["POSITION"]]
                                    let l = V3f acc.Min
                                    let h = V3f acc.Max
                                    Box3d(V3d l, V3d h)
                                    
                                let mode =
                                    match p.Mode with
                                    | MeshPrimitive.ModeEnum.POINTS -> IndexedGeometryMode.PointList
                                    | MeshPrimitive.ModeEnum.LINES -> IndexedGeometryMode.LineList
                                    | MeshPrimitive.ModeEnum.LINE_STRIP -> IndexedGeometryMode.LineStrip
                                    | MeshPrimitive.ModeEnum.TRIANGLES -> IndexedGeometryMode.TriangleList
                                    | MeshPrimitive.ModeEnum.TRIANGLE_STRIP -> IndexedGeometryMode.TriangleStrip
                                    | m -> failwithf "bad mode: %A" m // TODO: convert to TriangleList indices??
                                    
                                let material =
                                    if p.Material.HasValue then
                                        let mid, _mat, _mapping = materials.[p.Material.Value]
                                        Some mid
                                    else
                                        None
                                let tcMapping =
                                    if p.Material.HasValue then
                                        let _mid, _mat, mapping = materials.[p.Material.Value]
                                        mapping
                                    else
                                        HashMap.empty
                                    
                                let texCoords =
                                    attributes |> Array.choose (fun (name, arr) ->
                                        if name.StartsWith "TEXCOORD_" then
                                            match System.Int32.TryParse (name.Substring 9) with
                                            | true, id ->
                                                match arr with
                                                | :? array<V2f> as arr ->
                                                    match HashMap.tryFind id tcMapping with
                                                    | Some sems -> Some (arr, sems)
                                                    | None -> None
                                                | _arr ->
                                                    None
                                            | _ -> None
                                        else
                                            None
                                    )
                                    
                                let index =
                                    match index with
                                    | null -> null
                                    | :? array<uint8> as arr -> Array.map int arr
                                    | :? array<uint16> as arr -> Array.map int arr
                                    | :? array<uint32> as arr -> Array.map int arr
                                    | _ -> failwithf "unexpected index-type: %A" index
                                    
                                let normals =
                                    match HashMap.tryFind "NORMAL" attributeMap with
                                    | Some (:? array<V3f> as normals) -> Some normals
                                    | _ -> None
                                    
                                let tangents =
                                    match HashMap.tryFind "TANGENT" attributeMap with
                                    | Some (:? array<V4f> as tangents) -> Some tangents
                                    | _ -> None
                                    
                                let colors =
                                    match HashMap.tryFind "COLOR_0" attributeMap with
                                    | Some (:? array<C4b> as cs) -> Some cs
                                    | _ -> None
                                    
                                let mesh =
                                    {
                                        Name            = if System.String.IsNullOrEmpty m.Name then None else Some m.Name
                                        BoundingBox     = bounds
                                        Mode            = mode
                                        Index           = if isNull index then None else Some index
                                        Positions       = position
                                        Normals         = normals
                                        Tangents        = tangents  
                                        TexCoords       = Array.toList texCoords
                                        Colors          = colors
                                    }
                                    
                                let mutable cacheHit = true
                                let id = geometryIds.GetOrCreate(mesh, fun _ -> cacheHit <- false; MeshId.New())
                                if cacheHit then Log.warn "yeah, cache hit"
                                Some { Mesh = id; Material = material }
                            | m ->
                                Log.warn "mesh has incompatible positions: %A" m
                                None
                             
                        )
                            
                    )
               
            arrayCache.Clear()
                
            let roots =
                
                let rec traverse (nid : int) : Node =
                    let node = model.Nodes.[nid]
                    let trafo = getTrafo node
                    
                    let cs =
                        if isNull node.Children then []
                        else node.Children |> Array.toList |> List.map traverse
                        
                        
                    let geometry =
                        if node.Mesh.HasValue then
                            meshes.[node.Mesh.Value] |> Array.toList
                        else
                            []
                        
                    let name = if System.String.IsNullOrEmpty node.Name then None else Some node.Name
                
                    {
                        Name = name
                        Trafo = trafo
                        Children = cs
                        Meshes = geometry
                    }
                    
                if model.Scene.HasValue then
                    let scene = model.Scenes.[model.Scene.Value]
                    scene.Nodes |> Array.map (fun r ->
                        traverse r
                    )
                else
                    [||]
                
            let root = 
                match roots with
                | [||] -> { Name = None; Trafo = None; Meshes = []; Children = [] }
                | [|r|] -> r
                | rs -> { Name = None; Trafo = None; Meshes = []; Children = Array.toList rs }
                
                
            {
                Meshes      = geometryIds |> Seq.map (fun (KeyValue(a,b)) -> b, a) |> HashMap.ofSeq
                Materials   = materials |> Array.map (fun (a,b,_) -> a,b) |> HashMap.ofArray
                ImageData   = images |> Array.choose id |> HashMap.ofArray
                RootNode    = root
            }
     
    let load (file : string) =
        let model = Interface.LoadModel file
        let read (url : string) =
            if System.String.IsNullOrEmpty url then Interface.LoadBinaryBuffer(file)
            else
                let p = System.IO.Path.Combine(System.IO.Path.GetDirectoryName file, url)
                if System.IO.File.Exists p then System.IO.File.ReadAllBytes p
                else null
        toScene read model
            
    let tryLoad (file : string) =
        try Some (load file)
        with _ -> None

    let ofZipArchive (arch : System.IO.Compression.ZipArchive) =
        let entry =
            arch.Entries |> Seq.tryFind (fun e ->
                let n = e.Name.ToLower()
                n.EndsWith ".gltf" || n.EndsWith ".glb"
            )
        
        match entry with
        | Some entry ->
            let data =
                use s = entry.Open()
                s.ReadToEnd()
                
            use ms = new System.IO.MemoryStream(data)
                
            let model =
                Interface.LoadModel ms
                
            let read (url : string) =
                if System.String.IsNullOrEmpty url then
                    use s = new System.IO.MemoryStream(data)
                    Interface.LoadBinaryBuffer(s)
                else
                    match arch.Entries |> Seq.tryFind (fun e -> e.Name = url) with
                    | Some e ->
                        use s = e.Open()
                        s.ReadToEnd()
                    | None ->
                        printfn "NOT FOUND: %A" url
                        null
                        
            toScene read model
        | None ->
            failwith "no gltf/glb file found in zip"
         
    let tryOfZipArchive (arch : System.IO.Compression.ZipArchive) =
        try Some (ofZipArchive arch)
        with _ -> None

    let readFrom (input : System.IO.Stream) =
        if input.CanSeek then
            let model = Interface.LoadModel (new UnclosableStream(input))
            
            let read (url : string) =
                if System.String.IsNullOrEmpty url then
                    let o = input.Position
                    try
                        input.Position <- 0L
                        Interface.LoadBinaryBuffer (new UnclosableStream(input))
                    finally
                        input.Position <- o
                else
                    null
                    
            toScene read model
        else
            let arr = input.ReadToEnd()
            use ms = new System.IO.MemoryStream(arr)
            let model = Interface.LoadModel ms
            
            let read (url : string) =
                if System.String.IsNullOrEmpty url then
                    use ms = new System.IO.MemoryStream(arr)
                    Interface.LoadBinaryBuffer(input)
                else
                    null
                    
            toScene read model
    
    let tryReadFrom (input : System.IO.Stream) =
        try Some (readFrom input)
        with _ -> None
        
    let writeTo (binary : bool) (targetStream : System.IO.Stream) (scene : Scene) =
        let g = Gltf()
        
        let textures = scene.ImageData |> HashMap.toArray
        
        let rec getAllMeshInstances (n : Node) =
            n.Meshes @ (n.Children |> List.collect getAllMeshInstances)
        
        let meshInstances = HashSet.ofList (getAllMeshInstances scene.RootNode) |> HashSet.toArray
 
        let bufferViews = System.Collections.Generic.List()
        let accessors = System.Collections.Generic.List()
        let meshes = System.Collections.Generic.List()
        let materials = System.Collections.Generic.List()
        let materialTable = Dict<MaterialId * list<HashSet<TextureSemantic>>, option<int>>()
        
        let mutable arrayData = Array.zeroCreate<byte> (1 <<< 20)
        let mutable arrayLength = 0
        let mutable elementData = Array.zeroCreate<byte> (1 <<< 20)
        let mutable elementLength = 0
        let mutable meshPrimitives = HashMap.empty
        let nodes = System.Collections.Generic.List()
        let textureIndices = textures |> Array.mapi (fun i (tid, _) -> tid, i) |> HashMap.ofArray
        
        let align8 (v : int)  =
            let r = v &&& 7
            if r = 0 then v
            else 8 + (v - r)
        
        let bufferCache = Dict<System.Array * bool, int * int * int>()
        let accessorCache = Dict<System.Array * bool, int>()
        
        let inline getBuffer (element : bool) (data : 'a[]) =
            bufferCache.GetOrCreate((data, element), fun _ ->
                if element then
                    let arrayData = ()
                    let arrayLength = ()
                    use ptr = fixed data
                    let src = System.Span<byte>(NativePtr.toVoidPtr ptr, data.Length * sizeof<'a>)
                    
                    let offset = align8 elementLength
                    
                    if offset + src.Length > elementData.Length then
                        System.Array.Resize(&elementData, Fun.NextPowerOfTwo(offset + src.Length))
                    
                    let dst = System.Span<byte>(elementData, offset, src.Length)
                    src.CopyTo dst
                    elementLength <- offset + src.Length
                    1, offset, src.Length
                else
                    let elementData = ()
                    let elementLength = ()
                    use ptr = fixed data
                    let src = System.Span<byte>(NativePtr.toVoidPtr ptr, data.Length * sizeof<'a>)
                    
                    let offset = align8 arrayLength
                    
                    if offset + src.Length > arrayData.Length then
                        System.Array.Resize(&arrayData, Fun.NextPowerOfTwo(offset + src.Length))
                        
                    let dst = System.Span<byte>(arrayData, offset, src.Length)
                    src.CopyTo dst
                    arrayLength <- offset + src.Length
                    0, offset, src.Length
            )
            
        let inline getAccessor (element : bool) (data : 'a[]) =
            accessorCache.GetOrCreate((data, element), fun _ ->
                let t = typeof<'a>
                let view = glTFLoader.Schema.BufferView()
                
                let bid, offset, size = getBuffer element data
                
                view.Buffer <- bid
                view.ByteOffset <- offset
                view.ByteLength <- size
                view.Target <- if element then BufferView.TargetEnum.ELEMENT_ARRAY_BUFFER else BufferView.TargetEnum.ARRAY_BUFFER
                let vid = bufferViews.Count
                bufferViews.Add view
                
                let acc = Accessor()
                acc.BufferView <- System.Nullable vid
                acc.Count <- data.Length
                acc.ByteOffset <- 0
                acc.Normalized <-
                    t = typeof<C3b> || t = typeof<C4b> ||
                    t = typeof<C3us> || t = typeof<C4us> ||
                    t = typeof<C3ui> || t = typeof<C4ui>
                match data :> obj with
                | :? array<V3f> as data ->
                    let bounds = Box3f data
                    acc.Min <- bounds.Min.ToArray()
                    acc.Max <- bounds.Max.ToArray()
                | _ ->
                    ()
                acc.ComponentType <-
                    if t = typeof<float32> then Accessor.ComponentTypeEnum.FLOAT
                    elif t = typeof<int8> then Accessor.ComponentTypeEnum.BYTE
                    elif t = typeof<uint8> then Accessor.ComponentTypeEnum.UNSIGNED_BYTE
                    elif t = typeof<int16> then Accessor.ComponentTypeEnum.SHORT
                    elif t = typeof<uint16> then Accessor.ComponentTypeEnum.UNSIGNED_SHORT
                    elif t = typeof<int> then Accessor.ComponentTypeEnum.UNSIGNED_INT
                    elif t = typeof<uint32> then Accessor.ComponentTypeEnum.UNSIGNED_INT
                    
                    elif t = typeof<V2f> then Accessor.ComponentTypeEnum.FLOAT
                    elif t = typeof<V2i> then Accessor.ComponentTypeEnum.UNSIGNED_INT
                    elif t = typeof<V2ui> then Accessor.ComponentTypeEnum.UNSIGNED_INT
                    
                    elif t = typeof<V3f> then Accessor.ComponentTypeEnum.FLOAT
                    elif t = typeof<V3i> then Accessor.ComponentTypeEnum.UNSIGNED_INT
                    elif t = typeof<V3ui> then Accessor.ComponentTypeEnum.UNSIGNED_INT
                    elif t = typeof<C3b> then Accessor.ComponentTypeEnum.UNSIGNED_BYTE
                    elif t = typeof<C3us> then Accessor.ComponentTypeEnum.UNSIGNED_SHORT
                    elif t = typeof<C3ui> then Accessor.ComponentTypeEnum.UNSIGNED_INT
                    elif t = typeof<C3f> then Accessor.ComponentTypeEnum.FLOAT
                    
                    elif t = typeof<V4f> then Accessor.ComponentTypeEnum.FLOAT
                    elif t = typeof<V4i> then Accessor.ComponentTypeEnum.UNSIGNED_INT
                    elif t = typeof<V4ui> then Accessor.ComponentTypeEnum.UNSIGNED_INT
                    elif t = typeof<C4b> then Accessor.ComponentTypeEnum.UNSIGNED_BYTE
                    elif t = typeof<C4us> then Accessor.ComponentTypeEnum.UNSIGNED_SHORT
                    elif t = typeof<C4ui> then Accessor.ComponentTypeEnum.UNSIGNED_INT
                    elif t = typeof<C4f> then Accessor.ComponentTypeEnum.FLOAT
                    
                    else failwithf "Unsupported type: %A" t
                acc.Type <-
                    if t = typeof<float32> then Accessor.TypeEnum.SCALAR
                    elif t = typeof<int8> then Accessor.TypeEnum.SCALAR
                    elif t = typeof<uint8> then Accessor.TypeEnum.SCALAR
                    elif t = typeof<int16> then Accessor.TypeEnum.SCALAR
                    elif t = typeof<uint16> then Accessor.TypeEnum.SCALAR
                    elif t = typeof<int> then Accessor.TypeEnum.SCALAR
                    elif t = typeof<uint32> then Accessor.TypeEnum.SCALAR
                    
                    elif t = typeof<V2f> then Accessor.TypeEnum.VEC2
                    elif t = typeof<V2i> then Accessor.TypeEnum.VEC2
                    elif t = typeof<V2ui> then Accessor.TypeEnum.VEC2
                    
                    elif t = typeof<V3f> then Accessor.TypeEnum.VEC3
                    elif t = typeof<V3i> then Accessor.TypeEnum.VEC3
                    elif t = typeof<V3ui> then Accessor.TypeEnum.VEC3
                    elif t = typeof<C3b> then Accessor.TypeEnum.VEC3
                    elif t = typeof<C3us> then Accessor.TypeEnum.VEC3
                    elif t = typeof<C3ui> then Accessor.TypeEnum.VEC3
                    elif t = typeof<C3f> then Accessor.TypeEnum.VEC3
                    
                    elif t = typeof<V4f> then Accessor.TypeEnum.VEC4
                    elif t = typeof<V4i> then Accessor.TypeEnum.VEC3
                    elif t = typeof<V4ui> then Accessor.TypeEnum.VEC3
                    elif t = typeof<C4b> then Accessor.TypeEnum.VEC3
                    elif t = typeof<C4us> then Accessor.TypeEnum.VEC3
                    elif t = typeof<C4ui> then Accessor.TypeEnum.VEC3
                    elif t = typeof<C4f> then Accessor.TypeEnum.VEC3
                    
                    else failwithf "Unsupported type: %A" t
                
                let id = accessors.Count
                accessors.Add acc
                id
            )
        
        let getMaterial (sems : list<'a * HashSet<TextureSemantic>>) (id : MaterialId)  =
            let key = (id, sems |> List.map snd)
            materialTable.GetOrCreate(key, fun (id, sems) ->
                
                let getTextureInfo (sem : TextureSemantic) (tex : option<ImageId>) =
                    match tex with
                    | Some tex ->
                        match sems |> List.tryFindIndex (fun set -> HashSet.contains sem set) with
                        | Some coordIndex ->
                            match HashMap.tryFind tex textureIndices with
                            | Some texIdx ->
                                let info = TextureInfo()
                                info.Index <- texIdx
                                info.TexCoord <- coordIndex
                                Some info
                            | None ->
                                None
                        | None ->
                            None
                    | None ->
                        None
                
                match HashMap.tryFind id scene.Materials with
                | Some mat ->
                    let res = glTFLoader.Schema.Material()
                    res.Name <- defaultArg mat.Name (string id)
                    res.DoubleSided <- mat.DoubleSided
                    res.AlphaMode <- if mat.Opaque then Material.AlphaModeEnum.OPAQUE else Material.AlphaModeEnum.BLEND
                    res.EmissiveFactor <- mat.EmissiveColor.ToArray() |> Array.take 3
                    match getTextureInfo TextureSemantic.Emissive mat.EmissiveTexture with
                    | Some info -> res.EmissiveTexture <- info
                    | None -> ()
                    
                    res.PbrMetallicRoughness <- MaterialPbrMetallicRoughness()
                    res.AlphaCutoff <- 0.01f
                    res.PbrMetallicRoughness.MetallicFactor <- float32 mat.Metallicness
                    res.PbrMetallicRoughness.RoughnessFactor <- float32 mat.Roughness
                    res.PbrMetallicRoughness.BaseColorFactor <- mat.BaseColor.ToArray()
                    match getTextureInfo TextureSemantic.BaseColor mat.BaseColorTexture with
                    | Some tex -> res.PbrMetallicRoughness.BaseColorTexture <- tex
                    | None -> ()
                    
                    
                    match mat.RoughnessTexture with
                    | Some roughness when mat.RoughnessTextureComponent = 1 ->
                        match mat.MetallicnessTexture with
                        | Some metalness when mat.MetallicnessTextureComponent = 2 && roughness = metalness ->
                            // texture prepared for GLTF
                            let info = getTextureInfo TextureSemantic.Roughness mat.RoughnessTexture
                            match info with
                            | Some info ->
                                res.PbrMetallicRoughness.MetallicRoughnessTexture <- info
                            | None ->
                                ()
                        | _ ->
                            failwith "TODO: convert metalness/roughness textures to GLTF format"
                    | Some _ ->
                        failwith "TODO: convert metalness/roughness textures to GLTF format"
                    | None ->
                        match mat.MetallicnessTexture with
                        | Some _ ->
                            failwith "TODO: convert metalness/roughness textures to GLTF format"
                        | None ->
                            ()
                    match getTextureInfo TextureSemantic.Roughness mat.RoughnessTexture with
                    | Some tex -> res.PbrMetallicRoughness.MetallicRoughnessTexture <- tex
                    | None -> ()
                    
                    match getTextureInfo TextureSemantic.Normal mat.NormalTexture with
                    | Some info -> res.NormalTexture <- MaterialNormalTextureInfo(Index = info.Index, TexCoord = info.TexCoord, Scale = float32 mat.NormalTextureScale)
                    | None -> ()
                    
                    
                    let id = materials.Count
                    materials.Add res
                    Some id
                | None ->
                    None
            )

        for instance in meshInstances do
            match HashMap.tryFind instance.Mesh scene.Meshes with
            | Some mesh ->
                let matIndex = instance.Material |> Option.bind (getMaterial mesh.TexCoords)
                
                let res = glTFLoader.Schema.MeshPrimitive()
  
                
                match mesh.Index with
                | Some idx -> res.Indices <- System.Nullable (getAccessor true idx)
                | None -> ()
                
                res.Attributes <- System.Collections.Generic.Dictionary()
                res.Attributes.["POSITION"] <- getAccessor false mesh.Positions
                match mesh.Normals with
                | Some ns -> res.Attributes.["NORMAL"] <- getAccessor false ns
                | None -> ()
                match mesh.Tangents with
                | Some ns -> res.Attributes.["TANGENT"] <- getAccessor false ns
                | None -> ()
                match mesh.Colors with
                | Some ns -> res.Attributes.["COLOR_0"] <- getAccessor false ns
                | None -> ()
                
                let mutable idx = 0
                for (tc, _) in mesh.TexCoords do
                    let tc = tc |> Array.map (fun t -> V2f(t.X, 1.0f - t.Y))
                    res.Attributes.[sprintf "TEXCOORD_%d" idx] <- getAccessor false tc
                    idx <- idx + 1
                res.Mode <-
                    match mesh.Mode with
                    | IndexedGeometryMode.PointList -> MeshPrimitive.ModeEnum.POINTS
                    | IndexedGeometryMode.LineStrip -> MeshPrimitive.ModeEnum.LINE_STRIP
                    | IndexedGeometryMode.LineList -> MeshPrimitive.ModeEnum.LINES
                    | IndexedGeometryMode.TriangleStrip -> MeshPrimitive.ModeEnum.TRIANGLE_STRIP
                    | IndexedGeometryMode.TriangleList -> MeshPrimitive.ModeEnum.TRIANGLES
                    | m -> failwithf "bad mode: %A" m
                
                match matIndex with
                | Some idx -> res.Material <- System.Nullable idx
                | None -> ()
          
                meshPrimitives <- HashMap.add instance res meshPrimitives
                
            | None ->
                ()
            
        
        let rec run (node : Node) =
            let id = nodes.Count
            let res = glTFLoader.Schema.Node()
            nodes.Add res
            
            match node.Trafo with
            | Some t ->
                let (q, r) = QR.Decompose (t.Forward.UpperLeftM33())
                if not (Fun.IsTiny r.M01) || not (Fun.IsTiny r.M02) || not (Fun.IsTiny r.M12) then
                    let m = t.Forward
                    res.Matrix <-
                        [|
                            float32 m.M00; float32 m.M10; float32 m.M20; float32 m.M30
                            float32 m.M01; float32 m.M11; float32 m.M21; float32 m.M31
                            float32 m.M02; float32 m.M12; float32 m.M22; float32 m.M32
                            float32 m.M03; float32 m.M13; float32 m.M23; float32 m.M33
                        |]
                else
                    let s = r.Diagonal
                    let rot = Rot3d.FromM33d(q)
                    let t = t.TransformPos(V3d.Zero)
                    if not (Fun.ApproximateEquals(s, V3d.III)) then
                        res.Scale <- (V3f s).ToArray()
                    if not (Fun.ApproximateEquals(rot, Rot3d.Identity)) && not (Fun.ApproximateEquals(-rot, Rot3d.Identity)) then
                        res.Rotation <- [| float32 rot.X; float32 rot.Y; float32 rot.Z; float32 rot.W  |]
                    if not (Fun.ApproximateEquals(t, V3d.Zero)) then
                        res.Translation <- (V3f t).ToArray()
            | None -> ()
            
            let meshPrimitives = 
                node.Meshes |> List.choose (fun inst ->
                    HashMap.tryFind inst meshPrimitives    
                )
            
            match meshPrimitives with
            | [] -> ()
            | prims ->
                let resMesh = Mesh()
                resMesh.Primitives <- List.toArray prims
                let idx = meshes.Count
                meshes.Add resMesh
                res.Mesh <- idx
                
            if not (List.isEmpty node.Children) then
                res.Children <- node.Children |> List.map run |> List.toArray
           
            id
        
        
        let root = run scene.RootNode
        
        
        let inline octetString (data : byte[]) (offset : int) (length : int) =
            let b = System.Text.StringBuilder()
            b.Append "data:application/octet-stream;base64," |> ignore
            b.Append (System.Convert.ToBase64String(data, offset, length)) |> ignore
            b.ToString()
        
        let scene = glTFLoader.Schema.Scene()
        scene.Nodes <- [| root |]
        
        if textures.Length > 0 then
            g.Images <- 
                textures |> Array.mapi (fun i (tid, data) ->
                    let img = glTFLoader.Schema.Image()
                    match data.Name with
                    | Some n ->  img.Name <- n
                    | None -> ()
                    
                    match data.MimeType with
                    | Some "image/jpeg" -> img.MimeType <- Image.MimeTypeEnum.image_jpeg
                    | Some "image/png" -> img.MimeType <- Image.MimeTypeEnum.image_png
                    | _ ->
                        let data = data.Data
                        let isJpeg =  data.Length >= 10 && data.[0..1] = [| 0xFFuy; 0xD8uy |] && data.[6..9] = [| 0x4Auy; 0x46uy; 0x49uy; 0x46uy |]
                        let isPng = data.Length >= 4 && data.[0..3] = [| 0x89uy; 0x50uy; 0x4Euy; 0x47uy |]
                        if isJpeg then img.MimeType <- Image.MimeTypeEnum.image_jpeg
                        elif isPng then img.MimeType <- Image.MimeTypeEnum.image_png
                        else Log.warn "bad MIME"
                        
                    let bid, offset, size = getBuffer false data.Data
                    
                    let view = glTFLoader.Schema.BufferView()
                    view.Buffer <- bid
                    view.ByteOffset <- offset
                    view.ByteLength <- size
                    view.Name <- sprintf "TextureBuffer%03d" i
                    let vid = bufferViews.Count
                    bufferViews.Add view
                    
                    img.BufferView <- vid
                    img
                )    
            
            
            g.Textures <-
                g.Images |> Array.mapi (fun i img ->
                    let tex = glTFLoader.Schema.Texture()
                    tex.Source <- System.Nullable i
                    tex
                )
            
        if bufferViews.Count > 0 then 
            g.BufferViews <- bufferViews.ToArray()
        if materials.Count > 0 then
            g.Materials <- materials.ToArray()
        if meshes.Count > 0 then
            g.Meshes <- meshes.ToArray()
        if accessors.Count > 0 then 
            g.Accessors <- accessors.ToArray()
        if nodes.Count > 0 then
            g.Nodes <- nodes.ToArray()
    
        g.Buffers <-
            [| 
                if arrayLength > 0 then
                    Buffer(ByteLength = arrayLength, Uri = octetString arrayData 0 arrayLength)
                if elementLength > 0 then
                    Buffer(ByteLength = elementLength, Uri = octetString elementData 0 elementLength)
            |]
   
        g.Scenes <- [| scene |]
        g.Scene <- System.Nullable 0
        g.Asset <- glTFLoader.Schema.Asset()
        g.Asset.Generator <- "Aardvark"
        g.Asset.Version <- "2.0"
        
        if binary then
            use w = new System.IO.BinaryWriter(new UnclosableStream(targetStream), System.Text.Encoding.UTF8, true)
            Interface.SaveBinaryModel(g, null, w)
        else
            g.SaveModel (new UnclosableStream(targetStream))
        
    let toArray (scene : Scene) =
        use ms = new System.IO.MemoryStream()
        writeTo true ms scene
        ms.ToArray()
        
    let toString (scene : Scene) =
        use ms = new System.IO.MemoryStream()
        writeTo false ms scene
        ms.Position <- 0L
        use r = new System.IO.StreamReader(ms, System.Text.Encoding.UTF8)
        r.ReadToEnd()
        
    let tryOfArray (data : byte[]) =
        use ms = new System.IO.MemoryStream(data)
        tryReadFrom ms
        
    let ofArray (data : byte[]) =
        use ms = new System.IO.MemoryStream(data)
        readFrom ms
                
    let tryOfString (data : string) =
        let bytes = System.Text.Encoding.UTF8.GetBytes data
        use ms = new System.IO.MemoryStream(bytes)
        tryReadFrom ms
        
    let ofString (data : string) =
        let bytes = System.Text.Encoding.UTF8.GetBytes data
        use ms = new System.IO.MemoryStream(bytes)
        readFrom ms
        
    let save (file : string) (scene : Scene) =
        let binary = System.IO.Path.GetExtension(file).ToLower() = ".glb"
        if System.IO.File.Exists file then System.IO.File.Delete file
        use w = System.IO.File.OpenWrite file
        writeTo binary w scene
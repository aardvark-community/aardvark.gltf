module Aardvark.GLTF.Tests

open NUnit.Framework
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open FSharp.Data.Adaptive
open System.IO
open System.IO.Compression

[<AutoOpen>]
module StreamExtensions =
    
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
    

let testScene =
    let mutable materials = HashMap.empty
    let mutable geometries = HashMap.empty
    let mutable nodes = []
        
    let gid =
        let prim = IndexedGeometryPrimitives.solidPhiThetaSphere (Sphere3d(V3d.Zero, 0.3)) 36 C4b.White
        let pos = prim.IndexedAttributes.[DefaultSemantic.Positions] :?> V3f[]
        
        let idx = prim.IndexArray :?> int[]
        
        let geometry = 
            {
                Name            = None
                BoundingBox     = Box3f.FromCenterAndSize(V3f.Zero, V3f.III * 0.6f) |> Box3d
                Mode            = IndexedGeometryMode.TriangleList
                Index           = Some idx
                Positions       = pos
                Normals         = Some (pos |> Array.map Vec.normalize)
                Tangents        = None
                TexCoords       = []
                Colors          = None
            }
        
        let gid = MeshId.New()
        geometries <- HashMap.add gid geometry geometries
        gid
    
    let steps = 8
    for ri in 0 .. steps - 1 do
        let mutable roughness = float ri / float (steps - 1) |> float32 |> float
        for mi in 0 .. steps - 1 do
            let mutable metalness = float mi / float (steps - 1) |> float32 |> float
            let offset = Trafo3d.Translation(float ri, float mi, 0.0)
            
            let mid = MaterialId.New()
            
            let material =  
                {
                    Name                = Some (sprintf "%.3f_%.3f" roughness metalness)
                    
                    DoubleSided         = true
                    Opaque              = true
                        
                    BaseColorTexture    = None
                    BaseColor           = C4f.Beige
                        
                    Roughness           = roughness
                    RoughnessTexture    = None
                    RoughnessTextureComponent = 1
                    
                    Metallicness        = metalness
                    MetallicnessTexture = None
                    MetallicnessTextureComponent = 2
                    
                    EmissiveColor       = C4f.Black
                    EmissiveTexture     = None
                    
                    NormalTexture       = None
                    NormalTextureScale  = 1.0
                }
            
            materials <- HashMap.add mid material materials
            nodes <- { Name = None; Trafo = Some offset; Meshes = [ { Mesh = gid; Material = Some mid } ]; Children = [] } :: nodes
    
    {
        Materials = materials
        Meshes = geometries
        ImageData = HashMap.empty
        RootNode = { Name = None; Trafo = None; Meshes = []; Children = nodes }
    }
 
type Marker = Marker
let selfAssembly = typeof<Marker>.Assembly
  
    
[<Test>]
let ``GLTF.toArray working``() =
    testScene |> GLTF.toArray |> ignore
    
[<Test>]
let ``GLTF.toString working``() =
    testScene |> GLTF.toString |> ignore
    
[<Test>]
let ``GLTF.readFrom working with GLB``() =
    let name = selfAssembly.GetManifestResourceNames() |> Array.find (fun n -> n.EndsWith "Avocado.glb")
    use s = selfAssembly.GetManifestResourceStream(name)
    GLTF.readFrom s |> ignore
    
[<Test>]
let ``GLTF.readFrom working with GLTF``() =
    let name = selfAssembly.GetManifestResourceNames() |> Array.find (fun n -> n.EndsWith "2CylinderEngine.gltf")
    use s = selfAssembly.GetManifestResourceStream(name)
    GLTF.readFrom s |> ignore
    
[<Test>]
let ``GLTF.ofString working``() =
    let name = selfAssembly.GetManifestResourceNames() |> Array.find (fun n -> n.EndsWith "2CylinderEngine.gltf")
    use s = selfAssembly.GetManifestResourceStream(name)
    use r = new StreamReader(s)
    let str = r.ReadToEnd()
    GLTF.ofString str |> ignore

[<Test>]
let ``GLTF.ofArray working``() =
    let name = selfAssembly.GetManifestResourceNames() |> Array.find (fun n -> n.EndsWith "2CylinderEngine.gltf")
    use s = selfAssembly.GetManifestResourceStream(name)
    let data = s.ReadToEnd()
    GLTF.ofArray data |> ignore

[<Test>]
let ``GLTF.ofZipArchive working``() =
    let name = selfAssembly.GetManifestResourceNames() |> Array.find (fun n -> n.EndsWith "Avocado.zip")
    use s = selfAssembly.GetManifestResourceStream(name)
    use arch = new System.IO.Compression.ZipArchive(s, ZipArchiveMode.Read)
    GLTF.ofZipArchive arch |> ignore

[<Test>]
let ``GLTF.load working``() =
    let file = Path.ChangeExtension(Path.GetTempFileName(), ".gltf")
    do
        let name = selfAssembly.GetManifestResourceNames() |> Array.find (fun n -> n.EndsWith "2CylinderEngine.gltf")
        use s = selfAssembly.GetManifestResourceStream(name)
        use dst = File.OpenWrite file
        s.CopyTo dst
    GLTF.load file |> ignore

[<Test>]
let ``GLTF.save working``() =
    let file = Path.ChangeExtension(Path.GetTempFileName(), ".gltf")
    try testScene |> GLTF.save file
    finally
        try File.Delete file with _ -> ()

[<Test>]
let ``GLTF roundtrip GLB``() =
    testScene |> GLTF.toArray |> GLTF.ofArray |> ignore
    
[<Test>]
let ``GLTF roundtrip GLTF``() =
    testScene |> GLTF.toString |> GLTF.ofString |> ignore




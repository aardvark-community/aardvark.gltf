namespace Aardvark.GLTF

open System.Threading
open Aardvark.Base
open Aardvark.Rendering
open FSharp.Data.Adaptive
open System.Runtime.CompilerServices


/// A unique Id used for Materials in the Scene
[<Struct; StructuredFormatDisplay("{AsString}")>]
type MaterialId private (value : int) =
    static let mutable currentId = -1
    static member New() = MaterialId(Interlocked.Increment(&currentId))
    override x.ToString() = string value
    member private x.AsString = x.ToString()
   
/// A unique Id used for Images in the Scene
[<Struct; StructuredFormatDisplay("{AsString}")>]
type ImageId private (value : int) =
    static let mutable currentId = -1
    static member New() = ImageId(Interlocked.Increment(&currentId))
    override x.ToString() = string value
    member private x.AsString = x.ToString()

/// A unique Id used for Meshes in the Scene
[<Struct; StructuredFormatDisplay("{AsString}")>]
type MeshId private (value : int) =
    static let mutable currentId = -1
    static member New() = MeshId(Interlocked.Increment(&currentId))
    override x.ToString() = string value
    member private x.AsString = x.ToString()
        
/// A union-type for the different types of textures
type TextureSemantic =
    | BaseColor
    | Roughness
    | Metallicness
    | Normal
    | Emissive

/// A simple PBR material reprensentation
type Material =
    {
        Name                            : option<string>
                    
        DoubleSided                     : bool
        Opaque                          : bool  
                
        BaseColorTexture                : option<ImageId>
        BaseColor                       : C4f
                
        Roughness                       : float
        RoughnessTexture                : option<ImageId>
        RoughnessTextureComponent       : int
            
        Metallicness                    : float
        MetallicnessTexture             : option<ImageId>
        MetallicnessTextureComponent    : int
        
        EmissiveColor                   : C4f
        EmissiveTexture                 : option<ImageId>
                    
        NormalTexture                   : option<ImageId>
        NormalTextureScale              : float
    }

/// Mesh representation
[<CustomEquality; NoComparison; StructuredFormatDisplay("{AsString}")>]
type Mesh =
    {
        Name            : option<string>
        BoundingBox     : Box3d
        Mode            : IndexedGeometryMode
        Index           : option<int[]>
        Positions       : V3f[]
        Normals         : option<V3f[]>
        Tangents        : option<V4f[]>
        TexCoords       : list<V2f[] * HashSet<TextureSemantic>>
        Colors          : option<C4b[]>
    }
    
    // custom-equality ensuring reference-equality for arrays
    
    override x.Equals o =
        let inline opt a b =
            match a with
            | Some a ->
                match b with
                | Some b -> System.Object.ReferenceEquals(a, b)
                | None -> false
            | None ->
                match b with
                | Some _ -> false
                | None -> true
                
        let rec listEq (a : list<V2f[] * HashSet<TextureSemantic>>) (b : list<V2f[] * HashSet<TextureSemantic>>) =
            match a with
            | (a0, sa0) :: a ->
                match b with
                | (b0, sb0) :: b ->
                    System.Object.ReferenceEquals(a0, b0) &&
                    sa0 = sb0 &&
                    listEq a b
                | _ ->
                    false
            | [] ->
                List.isEmpty b
                
                
        match o with
        | :? Mesh as o ->
            x.Name = o.Name &&
            x.BoundingBox = o.BoundingBox &&
            x.Mode = o.Mode &&
            opt x.Index o.Index &&
            System.Object.ReferenceEquals(x.Positions, o.Positions) &&
            opt x.Normals o.Normals &&
            opt x.Tangents o.Tangents &&
            listEq x.TexCoords o.TexCoords &&
            opt x.Colors o.Colors
        | _ ->
            false
    
    override x.GetHashCode() =
        HashCode.Combine(
            hash x.Name,
            hash x.BoundingBox,
            hash x.Mode,
            (match x.Index with | Some a -> RuntimeHelpers.GetHashCode a | None -> 0),
            RuntimeHelpers.GetHashCode x.Positions,
            (match x.Normals with | Some a -> RuntimeHelpers.GetHashCode a | None -> 0),
            (match x.Tangents with | Some a -> RuntimeHelpers.GetHashCode a | None -> 0),
            (x.TexCoords |> List.map (fun (arr, set) -> HashCode.Combine(RuntimeHelpers.GetHashCode arr, hash set)) |> List.fold (fun a b -> HashCode.Combine(a, b)) 0),
            (match x.Colors with | Some a -> RuntimeHelpers.GetHashCode a | None -> 0)
            
        )
    
    member private x.AsString = x.ToString()
    
    override x.ToString() =
        sprintf "{ Name = %A; BoundingBox = %s; Mode = %A; Index = %A; Positions = %A; Normals = %A; Tangents = %A; TexCoords = %A; Colors = %A }"
            x.Name
            (x.BoundingBox.ToString("0.0000"))
            x.Mode
            (x.Index |> Option.map Array.length)
            x.Positions.Length
            (x.Normals |> Option.map Array.length)
            (x.Tangents |> Option.map Array.length)
            (x.TexCoords |> List.map (fun (arr, sem) -> (arr.Length, sem)))
            (x.Colors |> Option.map Array.length)
 
type MeshInstance =
    {
        Mesh          : MeshId
        Material      : option<MaterialId>
    }
 
type Node =
    {
        Name            : option<string>
        Trafo           : option<Trafo3d>
        Meshes          : list<MeshInstance>
        Children        : list<Node>
    }

type ImageData =
    {
        Name        : option<string>
        Data        : byte[]
        MimeType    : option<string>
        Semantics   : HashSet<TextureSemantic>
    }

type Scene =
    {
        Materials   : HashMap<MaterialId, Material>
        Meshes      : HashMap<MeshId, Mesh>
        ImageData   : HashMap<ImageId, ImageData>
        RootNode    : Node
    }
    member x.BoundingBox =
        let rec traverse (n : Node) : Box3d =
            let mutable box = n.Children |> List.map traverse |> Box3d
            for mi in n.Meshes do
                match HashMap.tryFind mi.Mesh x.Meshes with
                | Some m -> box.ExtendBy m.BoundingBox
                | None -> ()
            
            match n.Trafo with
            | Some t -> box.Transformed t
            | None -> box

        traverse x.RootNode

    
module Node =
    
    let empty =
        {
            Name = None
            Trafo = None
            Meshes = []
            Children = []
        }

    let ofList (children : list<Node>) =
        {
            Name = None
            Trafo = None
            Meshes = []
            Children = children
        }
        
    let ofSeq (children : seq<Node>) =
        children |> Seq.toList |> ofList
    
    let ofArray (children : Node[]) =
        children |> Array.toList |> ofList
    
    let ofMeshes (meshes : list<MeshInstance>) =
        {
            Name = None
            Trafo = None
            Meshes = meshes
            Children = []
        }
    
    let trafo (t : Trafo3d) (n : Node) =
        match n.Trafo with
        | Some o ->
            { n with Trafo = Some (t * o) }
        | None ->
            { n with Trafo = Some t }
    
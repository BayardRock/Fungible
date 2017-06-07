module Fungible.Paths

open System
open System.Reflection

open Microsoft.FSharp.Reflection

open Fungible.Helpers.CollectionHelpers


module internal Pathing = 

    let rec getPathsFor (mtype: Type) (path: string list) (yieldedParent: bool) = 
        seq {
            match mtype with 
            | IsOptionType t -> 
                yield path, t
                let et = t.GetGenericArguments().[0]
                yield! getPathsFor et path true
            | _ when FSharpType.IsRecord mtype -> 
                if not yieldedParent then yield path, mtype
                yield! getRecordPaths mtype path
            | _ when mtype.IsValueType || mtype = typeof<String> -> 
                if not yieldedParent then yield path, mtype
            | _ when mtype.IsArray ->
                if not yieldedParent then yield path, mtype
                yield! genArrayPaths mtype path
            | IsMapType _ -> yield path, mtype 
            | _ when mtype = typeof<System.Object> -> yield path, mtype
            | _ when FSharpType.IsUnion mtype  -> yield path, mtype
            | _ -> failwithf "Unexpected Type in Path: %s" (mtype.ToString())
        }

    and getRecordPaths (rtype: Type) (path: string list) = 
        seq {
            let pathedFields = 
                FSharpType.GetRecordFields(rtype) 
                |> Array.toList
                |> List.map (fun field -> field.Name :: path, field.PropertyType)
            for (path, ptype) in pathedFields do
                yield! getPathsFor ptype path false
        }

    and genArrayPaths (atype : Type) (path: string list)  = 
        let etype = atype.GetElementType()        
        seq {
            yield path, atype
            yield! getPathsFor etype path true
        }

open Pathing

/// <summary>
/// Returns a map of the valid paths with type for a given input type.
/// </summary>
let getPathsAndTypes<'t> () = 
    let rtype = typeof<'t>
    getPathsFor rtype [] true |> Map.ofSeq
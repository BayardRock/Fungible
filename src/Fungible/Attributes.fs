module Fungible.Attributes

open System

open System
open System.Reflection
open System.Linq

[<System.AttributeUsage(System.AttributeTargets.Method)>]
type CleaningFunctionTypeAttribute (ftype: String) =
    inherit System.Attribute()
    member val Type = ftype with get, set

let getFunctionType (mi: MethodInfo) =
    let attribs = 
        mi.GetCustomAttributes(typeof<CleaningFunctionTypeAttribute>)
        |> Seq.toList
    match attribs with
    | [] -> None
    | (:? CleaningFunctionTypeAttribute as attr) :: [] -> Some attr
    | _ :: _ -> failwith "More than one CleaningFunctionTypeAttribute found!"

let getFunctionFromName (fn: string) (fmod: Type) =
    fmod.GetMethod(fn)

/// Gets the cleaning function attribute CleaningFunctionTypeAttribute
let internal getCleaningFunctionAttr (m: MethodInfo) = 
    let attr = m.GetCustomAttribute(typeof<CleaningFunctionTypeAttribute>)
    if attr = null then None else Some (attr :?> CleaningFunctionTypeAttribute, m)

/// Gets all data cleaners that are marked with a CleaningFunctionTypeAttribute
let GatherAllDataCleaners(moduleType: Type) = 
    moduleType.GetMethods()
        .Select(getCleaningFunctionAttr)
        .Where(fun x -> x.IsSome)
        .Select(fun x -> x.Value)
        
type BasicCleaningInputFile<'t> =
    {
        Contents: 't
    }
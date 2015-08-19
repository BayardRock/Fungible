module Fungible.Attributes

open System

open System
open System.Reflection
open System.Linq

/// <summary>
/// Used to tag F# functions with the kind of transform they represent 
/// </summary>
[<System.AttributeUsage(System.AttributeTargets.Method)>]
type TransformFunctionTypeAttribute (ftype: String) =
    inherit System.Attribute()
    member val Type = ftype with get, set

/// <summary>
/// Gets the transform type from a given MethodInfo as given by a TransformFunctionTypeAttribute 
/// </summary>
/// <param name="mi">The MethodInfo representing the function you'd like to query</param>
let getFunctionType (mi: MethodInfo) =
    let attribs = 
        mi.GetCustomAttributes(typeof<TransformFunctionTypeAttribute>)
        |> Seq.toList
    match attribs with
    | [] -> None
    | (:? TransformFunctionTypeAttribute as attr) :: [] -> Some attr
    | _ :: _ -> failwith "More than one CleaningFunctionTypeAttribute found!"

/// <summary>
/// Gets a MethodInfo for a function (as a string) in a given module
/// </summary>
/// <param name="functionName">The name of the function</param>
/// <param name="functionModule">The type of the module the function lives in</param>
let getFunctionFromName (functionName: string) (functionModule: Type) =
    functionModule.GetMethod(functionName)

/// A Helper function for getting the TransformFunctionTypeAttribute
let internal getTransformFunctionAttribute (m: MethodInfo) = 
    let attr = m.GetCustomAttribute(typeof<TransformFunctionTypeAttribute>)
    if attr = null then None else Some (attr :?> TransformFunctionTypeAttribute, m)

/// Gets all data transforms that are marked with a CleaningFunctionTypeAttribute in the given module
let GatherAllDataTransforms(moduleType: Type) = 
    moduleType.GetMethods() |> Array.choose getTransformFunctionAttribute
        

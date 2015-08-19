module Fungible.Barb

open System
open System.ComponentModel
open System.Collections.Generic

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

open Fungible
open Fungible.Core
open Fungible.Simple

open Barb.Compiler
open Barb.Representation

open Fungible.Simple.Types

/// <summary>
/// Defines a Barb function as a transformation to apply
/// </summary>
[<CLIMutable>]
type BarbTransformDefinition = 
    {
        /// The path to the target field for this cleaner
        [<Description("The path to the target field for this cleaner")>]
        TargetPath: string
        /// The kind of operation to be performed on the target field (map, filter, collect, default, function, or add
        [<Description("The kind of operation to be performed on the target field (map, filter, collect, default, function, or add")>]
        Kind: string
        /// The Barb function to be used on the given field
        [<Description("The Barb function to be used on the given field")>]
        Function: string
    }
    with override t.ToString () = sprintf "%s with %s of \"%s\"" t.TargetPath t.Kind t.Function

/// A wrapper used to provide an extra argument to the Barb function
type ValueContainer2<'u,'t> = { Scope: 'u; Value: 't }
/// Used to create ValueContainer2 in scope in Barb
let MkValueContainer2 u t = { Scope = u; Value = t }

module internal BarbTransformWrappers =
    open CollectionHelpers
    open ExprHelpers

    let mapToKeyValueArray (m: Map<'k,'v>) =
        m |> Map.toArray |> Array.map (fun (k,v) -> new KeyValuePair<_,_>(k,v))

    let keyValueArrayToMap (kva: KeyValuePair<'k,'v> []) =
        kva |> Array.map (fun kv -> kv.Key, kv.Value) |> Map.ofArray

    let tupleToKeyValue (tv: System.Tuple<'k, 'v>) = new KeyValuePair<_,_>(tv.Item1, tv.Item2)

    let keyValueToTuple (kv: KeyValuePair<'k,'v>) = kv.Key, kv.Value

    let getTypeWrapper (targetType: Type) (tk: TypeKind) =
        match targetType, tk with
        | IsMapType t, Inner -> 
            let args = targetType.GetGenericArguments()
            let kvpt = typeof<KeyValuePair<_,_>>.GetGenericTypeDefinition().MakeGenericType(args)
            let t2kv = (getMethod <@ tupleToKeyValue X @>).MakeGenericMethod(args)
            let kv2t = (getMethod <@ keyValueToTuple X @>).MakeGenericMethod(args)
            Some (kvpt, t2kv, kv2t)
        | IsMapType t, Outter ->        
            let args = targetType.GetGenericArguments()
            let kvpt = typeof<KeyValuePair<_,_>>.GetGenericTypeDefinition().MakeGenericType(args)
            let kvpat = typeof<_ []>.GetGenericTypeDefinition().MakeGenericType(kvpt)
            let m2kva = (getMethod <@ mapToKeyValueArray X @>).MakeGenericMethod(args)
            let kva2m = (getMethod <@ keyValueArrayToMap X @>).MakeGenericMethod(args)
            Some (kvpat, m2kva, kva2m) 
        | _ -> None       

module internal Internals = 
    open ExprHelpers

    let makeBarbFunction (barbSettings: BarbSettings) (recordType: Type) (inputType: Type) (outputType: Type) (textFunc: string) : obj -> obj =
        let wrappedInType = typeof<ValueContainer2<_,_>>.GetGenericTypeDefinition().MakeGenericType(recordType, inputType)
        Barb.Compiler.buildUntypedExprWithSettings (wrappedInType) (outputType) barbSettings textFunc

    let generateAdvancedCleaner (barbSettings: BarbSettings) (funcStr: string) (funcType: string) (recordType: Type) (targetType: Type) =
        let fk = getFunctionKind funcType
        let actualInType = getActualType targetType fk.InputKind
    
        let barbInputType, barbInputConverter =
            match BarbTransformWrappers.getTypeWrapper targetType fk.InputKind with 
            | None -> actualInType, None
            | Some (bit, tobit, frombit) -> bit, Some tobit

        let barbOutputType, barbOutputConverter = 
            let actualOutType = getActualType targetType fk.OutputKind
            match BarbTransformWrappers.getTypeWrapper targetType fk.OutputKind with  
            | None -> actualOutType, None
            | Some (bit, tobit, frombit) -> bit, Some frombit

        let arg = Var("x", actualInType, false)

        let useReplaceArg = Expr.Var(Var("replaceMe", recordType, false))
        let useArg = Expr.Var(arg)

        let contents = 
            let wrappedBarb = makeBarbFunction barbSettings recordType barbInputType barbOutputType funcStr
            let mkValueContainerExpr (inputArg: Expr) = 
                let vcExpr = (getMethod <@ MkValueContainer2 X X @>).MakeGenericMethod([|recordType; barbInputType|])
                Expr.Call(vcExpr, [useReplaceArg; inputArg ])
            let mkBarbContents (vcx: Expr) = application (vcx |> coerse typeof<obj>) <@ wrappedBarb @> |> coerse barbOutputType
            match barbInputConverter, barbOutputConverter with
            | None, None -> mkValueContainerExpr useArg |> mkBarbContents
            | Some ic, None -> call ic [useArg] |> mkValueContainerExpr |> mkBarbContents
            | None, Some oc -> [(mkValueContainerExpr useArg |> mkBarbContents)] |> call oc
            | Some ic, Some oc -> [call ic [useArg] |> mkValueContainerExpr |> mkBarbContents] |> call oc
            
        Expr.Lambda(arg, contents) |> fk.ExprWrapper

open Internals

/// Suggested default Barb Settings
let defaultBarbSettings = 
    let barbNamespaces = 
        Barb.Representation.BarbSettings.Default.Namespaces
        |> Set.add "System.Linq"
        |> Set.add "System.Collections.Generic"
        |> Set.add "System.IO"
    { Barb.Representation.BarbSettings.Default with Namespaces = barbNamespaces }

/// <summary>
///  Creates a transform out of an Barb function as defined in a BarbTransformDefinition
/// </summary>
/// <param name="barbSettings">Settings to use for Barb when generating the transform</param>
/// <param name="propertyMap">A map of paths to types gotten from getPathsAndTypes<'t></param>
/// <param name="transformDef">The definition of the Barb function to be generated</param>
let generateBarbTransform<'U> (barbSettings: BarbSettings) (propertyMap: Map<string list, Type>) (transformDef: BarbTransformDefinition) =
    let path = nameToPath transformDef.TargetPath  
    let propertType = propertyMap.[path]
    let cleaner = generateAdvancedCleaner barbSettings transformDef.Function transformDef.Kind typeof<'U> propertType
    path, cleaner

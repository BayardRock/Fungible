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

[<CLIMutable>]
type BarbCleanerDefinition = 
    {
        [<Description("The target field for this cleaner")>]
        Field: string
        [<Description("The kind of operation to be performed on the target field")>]
        Type: string
        [<Description("The Barb function to be used on the given field")>]
        Function: string
    }
    with override t.ToString () = sprintf "%s with %s of \"%s\"" t.Field t.Type t.Function

type ValueContainer2<'u,'t> = { Scope: 'u; Value: 't }
let MkValueContainer2 u t = { Scope = u; Value = t }

module DataCleaningWrappers =

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

    let makeBarbFunction (barbSettings: BarbSettings) (recordType: Type) (inputType: Type) (outputType: Type) (textFunc: string) : obj -> obj =
        let wrappedInType = typeof<ValueContainer2<_,_>>.GetGenericTypeDefinition().MakeGenericType(recordType, inputType)
        Barb.Compiler.buildUntypedExprWithSettings (wrappedInType) (outputType) barbSettings textFunc

    let generateAdvancedCleaner (barbSettings: BarbSettings) (funcStr: string) (funcType: string) (recordType: Type) (targetType: Type) =
        let fk = getFunctionKind funcType
        let actualInType = getActualType targetType fk.InputKind
    
        let barbInputType, barbInputConverter =
            match DataCleaningWrappers.getTypeWrapper targetType fk.InputKind with 
            | None -> actualInType, None
            | Some (bit, tobit, frombit) -> bit, Some tobit

        let barbOutputType, barbOutputConverter = 
            let actualOutType = getActualType targetType fk.OutputKind
            match DataCleaningWrappers.getTypeWrapper targetType fk.OutputKind with  
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

let defaultBarbSettings = 
    let barbNamespaces = 
        Barb.Representation.BarbSettings.Default.Namespaces
        |> Set.add "System.Linq"
        |> Set.add "System.Collections.Generic"
        |> Set.add "System.IO"
    { Barb.Representation.BarbSettings.Default with Namespaces = barbNamespaces }

let generateBarbCleaner<'U> (barbSettings: BarbSettings) (advanced: BarbCleanerDefinition) (propertyMap: Map<string list, Type>) =
    let path = nameToPath advanced.Field  
    let propertType = propertyMap.[path]
    let cleaner = generateAdvancedCleaner barbSettings advanced.Function advanced.Type typeof<'U> propertType
    path, cleaner

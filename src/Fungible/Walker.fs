module Fungible.Walker

open Fungible.Helpers
open Fungible.Helpers.ExprHelpers
open Fungible.Helpers.CollectionHelpers

open System
open System.Reflection
open System.Linq.Expressions

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations

let returnNotUnitSecond (_: 'a) (v2: 'b) = v2
let sequenceExprs (expr1: Expr) (expr2: Expr) =
    let meth = (getMethod <@ returnNotUnitSecond X X @>).MakeGenericMethod([|expr1.Type; expr2.Type|])
    Expr.Call(meth, [expr1; expr2])

let makeSingleArgWalkExpr (mtype: Type) (path: string list) dispatchOnType : Expr =
    let arg = Var("x", mtype, false)
    let useArg = Expr.Var(arg)
    let contents = dispatchOnType mtype path useArg
    Expr.Lambda(arg, contents)

let genFieldWalker (instance1: Expr) (instance2: Expr) (field: PropertyInfo) (path: string list) dispatchOnType : Expr = 
    let p1val = Expr.PropertyGet(instance1, field) 
    let p2val = Expr.PropertyGet(instance2, field)
    dispatchOnType field.PropertyType path p1val p2val

let genRecordWalker (rtype: Type) (path: string list) (instance1: Expr) (instance2: Expr) dispatchOnType : Expr =  
    FSharpType.GetRecordFields(rtype) 
    |> Array.toList
    |> List.map (fun field -> field.Name :: path, field)
    |> List.map (fun (fpath, field) -> genFieldWalker instance1 instance2 field fpath dispatchOnType)
    |> List.reduce (fun e1 e2 ->  sequenceExprs e1 e2) 

let callFunAndCont (f: obj -> obj -> string list -> unit) (path: string list) (instance1: Expr) (instance2: Expr) (next: Expr) : Expr = 
    let compExpr = <@@ f %%instance1 %%instance2 path @@>
    sequenceExprs compExpr next

let rec dispatchOnType (f: obj -> obj -> string list -> unit) (mtype: Type) (path: string list) (instance1: Expr) (instance2: Expr) : Expr = 
    match mtype with 
//   | IsOptionType _ -> walkOption mtype path instance1 instance2 dispatchOnType
    | _ when FSharpType.IsRecord mtype -> 
                let walker = genRecordWalker mtype path instance1 instance2 (dispatchOnType f)
                callFunAndCont f path instance1 instance2 walker
                //genRecordWalker mtype path instance1 instance2 (dispatchOnType f)
//    | _ when FSharpType.IsUnion mtype  -> genUnionCopier rcs mtype funcs path instance
//    | _ when mtype.IsValueType || mtype = typeof<String> -> genSimpleValueCopy rcs mtype funcs path instance 
//    | _ when mtype.IsArray -> genArrayCopier rcs mtype funcs path instance
//    | IsMapType _ -> genMapCopier rcs mtype funcs path instance
//    | _ when FSharpType.IsTuple mtype -> genTupleCopyFunExpr rcs mtype funcs path instance
//    | _ when mtype = typeof<obj> -> instance 
//    | _ when rcs.FailOnUnsupportedType -> failwithf "Type not supported in record cloning: %s" mtype.FullName
//    | _ -> instance
﻿module Fungible.Walker

open Fungible.Helpers
open Fungible.Helpers.ExprHelpers
open Fungible.Helpers.CollectionHelpers
open Fungible.Helpers.Linq

open System
open System.Reflection
open System.Linq.Expressions

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations

// Note: Sequence N Exprs for speedup
let returnNotUnitSecond (_: 'a) (v2: 'b) = v2
let sequenceExprs (expr1: Expr) (expr2: Expr) =
    let meth = (getMethod <@ returnNotUnitSecond X X @>).MakeGenericMethod([|expr1.Type; expr2.Type|])
    Expr.Call(meth, [expr1; expr2])

let sequenceLambda (t1: Type) (t2: Type)=
    let instance1Var = Var("instance1", t1, false)
    let instance1Arg = Expr.Var(instance1Var)
    let instance2Var = Var("instance2", t2, false)
    let instance2Arg = Expr.Var(instance2Var)
    Expr.Lambda(instance1Var, Expr.Lambda(instance2Var, sequenceExprs instance1Arg instance2Arg))

//    /// Compare Application
//    let returnNotUnitSecond (_: 'a) (v2: 'b) = v2
//    let inline applyCompare (prmType: Type) (name: string list) prm1 prm2 compFunExpr = 
//        let inputs = Expr.NewTuple [Expr.Value (List.rev name); prm1; prm2]
//        let app = Expr.Application(compFunExpr, inputs)
//        let meth = (getMethod <@ returnNotUnitSecond X X @>).MakeGenericMethod([|typeof<unit>; prmType|])
//        Expr.Call(meth, [app; prm2])

//let makeSingleArgWalkExpr (mtype: Type) (path: string list) dispatchOnType : Expr =
//    let arg = Var("x", mtype, false)
//    let useArg = Expr.Var(arg)
//    let contents = dispatchOnType mtype path useArg
//    Expr.Lambda(arg, contents)

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
//    |> List.reduce (fun e1 e2 -> e1) 

let makeFuncionCall (f: obj -> obj -> string list -> unit) (path: string list) (instance1: Expr) (instance2: Expr) =
    let objExpr1 = Expr.Coerce(instance1, typeof<obj>)
    let objExpr2 = Expr.Coerce(instance2, typeof<obj>)
    <@@ f %%objExpr1 %%objExpr2 path @@>

let callFunAndCont (f: obj -> obj -> string list -> unit) (path: string list) (instance1: Expr) (instance2: Expr) (next: Expr) : Expr = 
    let functionExpr = makeFuncionCall f path instance1 instance2    
    sequenceExprs functionExpr next

let rec dispatchOnType (f: obj -> obj -> string list -> unit) (mtype: Type) (path: string list) (instance1: Expr) (instance2: Expr) : Expr = 
    match mtype with 
//   | IsOptionType _ -> walkOption mtype path instance1 instance2 dispatchOnType
    | _ when FSharpType.IsRecord mtype -> 
                let walker = genRecordWalker mtype path instance1 instance2 (dispatchOnType f)
                callFunAndCont f path instance1 instance2 walker
                //genRecordWalker mtype path instance1 instance2 (dispatchOnType f)
//    | _ when FSharpType.IsUnion mtype  -> genUnionCopier rcs mtype funcs path instance
    | _ when mtype.IsValueType || mtype = typeof<String> -> makeFuncionCall f path instance1 instance2

//    | _ when mtype.IsArray -> genArrayCopier rcs mtype funcs path instance
//    | IsMapType _ -> genMapCopier rcs mtype funcs path instance
//    | _ when FSharpType.IsTuple mtype -> genTupleCopyFunExpr rcs mtype funcs path instance
//    | _ when mtype = typeof<obj> -> instance 
//    | _ when rcs.FailOnUnsupportedType -> failwithf "Type not supported in record cloning: %s" mtype.FullName
//    | _ -> instance      
      | _ -> failwithf "Unexpected type: %s" (mtype.ToString())

let makeSingleWalker (f: obj -> obj -> string list -> unit) (mtype: Type) (path: string list) : Expr =
    let arg1 = Var("x", mtype, false)
    let useArg1 = Expr.Var(arg1)
    let arg2 = Var("y", mtype, false)
    let useArg2 = Expr.Var(arg2)
    let contents = dispatchOnType f mtype path useArg1 useArg2
    Expr.Lambda(arg1, Expr.Lambda(arg2, contents))

open FSharp.Quotations.Evaluator

let generateWalker<'T> (f: obj -> obj -> string list -> unit) : ('T -> 'T -> unit) =
    let baseType = typeof<'T>

    //let contents = dispatchOnType f baseType [] instance1Arg instance2Arg
    let contents = makeSingleWalker f baseType []

    let castExpr : Expr<'T -> 'T -> unit> = contents |> Expr.Cast
    castExpr.Compile()
//    <@ fun (x: 'T) (y: 'T) -> () @>.Compile()
       
//    let linqExpr = castExpr |> toLinq2<'T,'T,unit>
//    let compiledExpr = linqExpr.Compile()
//    fun (i1: 'T) (i2 : 'T) -> compiledExpr.Invoke(i1, i2)


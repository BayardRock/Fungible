module Fungible.Helpers

open System

open Microsoft.FSharp.Quotations

module ExprHelpers = 
    /// Shortcut for Expr.Application
    let inline application prms expr = Expr.Application(expr, prms)
       
    /// Shortcut for Expr.Coerce
    let inline coerse typ expr = Expr.Coerce(expr, typ)
    /// Shortcut for Expr.NewRecord
    let inline newrec typ args = Expr.NewRecord(typ, args)
    /// Shortcut for Expr.Call    
    let inline call meth args = Expr.Call(meth, args)

    /// Makes it easy to get a MethodInfo out of a quotation. 
    /// x: let m = (getMethod <@ failwith message @>).MakeGenericMethod([|utype|])
    let getMethod = 
        function
        | Patterns.Call (_, m, _) when m.IsGenericMethod -> m.GetGenericMethodDefinition()
        | Patterns.Call (_, m, _) -> m
        | _ -> failwith "Incorrect getMethod Pattern"

    /// Makes it easy to get a placeholder value for a type used to be used in a quotation
    let X<'T> : 'T = Unchecked.defaultof<'T>

    /// Compare Application
    let returnNotUnitSecond (_: 'a) (v2: 'b) = v2
    let inline applyCompare (prmType: Type) (name: string list) prm1 prm2 compFunExpr = 
        let inputs = Expr.NewTuple [Expr.Value (List.rev name); prm1; prm2]
        let app = Expr.Application(compFunExpr, inputs)
        let meth = (getMethod <@ returnNotUnitSecond X X @>).MakeGenericMethod([|typeof<unit>; prmType|])
        Expr.Call(meth, [app; prm2])

module CollectionHelpers =

    let internal defaultOptionIfNull defaultfun option = if Option.isNone option then defaultfun() else option

    let internal defaultArrayIfEmpty defaultfun arr = if Array.isEmpty arr then defaultfun() else arr
    let internal addToArray addFun arr = 
        let r : _ [] = addFun ()
        if r.Length > 0 then Array.append arr r
        else arr

    let internal mapMapValues (mapFun: ('k*'v) -> ('k*'v)) (m : Map<'k,'v>) : Map<'k,'v> = 
        m |> Map.toSeq |> Seq.map mapFun |> Map.ofSeq

    let internal filterMap (filterFun: ('k * 'v) -> bool) (m: Map<'k,'v>) = 
         m |> Map.filter (fun k v -> filterFun (k, v))

    let internal filterOption (f: 'a -> bool) = 
        function
        | Some v when f v -> Some v
        | Some _ | None -> None

    let internal defaultMapIfEmpty (defaultfun: unit -> Map<'k,'v>) m = 
        if Map.isEmpty m then defaultfun() else m

    let internal addMap (addFun: unit -> Map<'k,'v>) m = 
        addFun() |> Map.fold (fun st k v -> st |> Map.add k v) m 

    let (|IsMapType|_|) (t: Type) = 
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Map<_,_>> then Some t
        else None    

    let (|IsOptionType|_|) (t: Type) =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then Some t
        else None

module Linq =
    
    open System
    open System.Reflection
    open System.Linq.Expressions

    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Linq.RuntimeHelpers

    let toLinq<'I,'O> (expr: Expr<'I -> 'O>) =
        let linq = LeafExpressionConverter.QuotationToExpression expr
        let call = linq :?> MethodCallExpression
        let lambda  = call.Arguments.[0] :?> LambdaExpression
        Expression.Lambda<Func<'I,'O>>(lambda.Body, lambda.Parameters)

    // Unnests F# Lamdbas in C# Linq Expressions
    let rec unnestLambdas (linq:Expression) = 
      match linq with
      | :? MethodCallExpression as mc ->
          let le = mc.Arguments.[0] :?> LambdaExpression
          let args, body = unnestLambdas le.Body
          le.Parameters.[0] :: args, body
      | _ -> [], linq

    let toLinq2<'I1, 'I2, 'O> (expr: Expr<'I1 -> 'I2 -> 'O>) =
        let linq = Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.QuotationToExpression expr
        let prms, body = linq :?> MethodCallExpression |> unnestLambdas
        Expression.Lambda<Func<'I1,'I2,'O>>(body, prms |> Array.ofList)
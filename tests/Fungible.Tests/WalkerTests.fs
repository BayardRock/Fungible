module Fungible.Walker.Tests

open System.Collections.Generic

open Xunit

open Fungible.Walker

type FlatRecord = 
    {   
        String: string
        Integer: int
        Float: float
    }

[<Fact>]
let ``walker should be able to visit all members of a simple record type`` () =
    let output = ResizeArray<string>()
    let walker (e1: obj) (e2: obj) (name: string list) =
        output.Add(sprintf "%A %A %A" e1 e2 name)
        
    let walkerFun = generateWalker<FlatRecord> WalkerSettings.Default walker
    walkerFun { String = "Str1"; Integer = 1; Float = 1.0 } { String = "Str2"; Integer = 2; Float = 2.0} 
    Assert.Equal(4, output.Count)

type TestPOCO (String: string, Integer: int, Float: float) =    
    member t.String = String
    member t.Intger = Integer
    member t.Float = Float

[<Fact>]
let ``walker should be able to visit all members of a simple POCO type`` () =
    let output = ResizeArray<string>()
    let walker (e1: obj) (e2: obj) (name: string list) =
        output.Add(sprintf "%A %A %A" e1 e2 name)
        
    let settings = { WalkerSettings.Default with CompareCSharpProperties = true }
    let walkerFun = generateWalker<TestPOCO> settings walker
    walkerFun (TestPOCO("Str1",1,1.0)) (TestPOCO("Str2",2,2.0))
    Assert.Equal(4, output.Count)

type TestSetProp (String: string, Integer: int, Float: float) =    
    member t.String = String
    member t.Integer = Integer
    member t.Float = Float
    member t.SetProp with set v = () 

[<Fact>]
let ``walker shouldn't expode on set only properties`` () =
    let output = ResizeArray<string>()
    let walker (e1: obj) (e2: obj) (name: string list) =        
        output.Add(sprintf "%A %A %A" e1 e2 name)
        
    let settings = { WalkerSettings.Default with CompareCSharpProperties = true }
    let walkerFun = generateWalker<TestSetProp> settings walker
    walkerFun (TestSetProp("Str1",1,1.0)) (TestSetProp("Str2",2,2.0))
    Assert.Equal(4, output.Count)

type TestUnion = 
    | String of string
    | Integer of int
    | Float of float

type TestUnionRecord = 
    {
       TestUnion: TestUnion
    }

[<Fact>]
let ``walker should compare the same basic union case properly`` () =
    let output = ResizeArray<string>()
    let walker (e1: obj) (e2: obj) (name: string list) =        
        output.Add(sprintf "%A %A %A" e1 e2 name)
        
    let settings = { WalkerSettings.Default with CompareCSharpProperties = true }
    let walkerFun = generateWalker<TestUnionRecord> settings walker
    walkerFun ({ TestUnion = String "one"}) ({ TestUnion = String "two"})
    Assert.Equal(3, output.Count) // one for the root, one for the member, one for inside

[<Fact>]
let ``walker shouldnt compare the same basic union case properly if different`` () =
    let output = ResizeArray<string>()
    let walker (e1: obj) (e2: obj) (name: string list) =        
        output.Add(sprintf "%A %A %A" e1 e2 name)
        
    let settings = { WalkerSettings.Default with CompareCSharpProperties = true }
    let walkerFun = generateWalker<TestUnionRecord> settings walker
    walkerFun ({ TestUnion = String "one"}) ({ TestUnion = Integer 2 })
    Assert.Equal(2, output.Count) // one for the root, one for the member

type TestUnion2 = 
    | EmbedFields of String: string * Integer: int * Float: float
    | EmbedRec    of FlatRecord

[<Fact>]
let ``walker should compare the same union case with members properly`` () =
    let output = ResizeArray<string>()
    let walker (e1: obj) (e2: obj) (name: string list) =        
        output.Add(sprintf "%A %A %A" e1 e2 name)
        
    let settings = { WalkerSettings.Default with CompareCSharpProperties = true }
    let walkerFun = generateWalker<TestUnion2> settings walker
    walkerFun (EmbedFields("one", 1, 1.0)) (EmbedFields("two", 2, 2.0))
    Assert.Equal(4, output.Count) // one for the root, one for each member

[<Fact>]
let ``walker should compare the same union case with records properly`` () =
    let output = ResizeArray<string>()
    let walker (e1: obj) (e2: obj) (name: string list) =        
        output.Add(sprintf "%A %A %A" e1 e2 name)
        
    let settings = { WalkerSettings.Default with CompareCSharpProperties = true }
    let walkerFun = generateWalker<TestUnion2> settings walker
    walkerFun (EmbedRec {String ="one"; Integer = 1; Float = 1.0}) (EmbedRec {String ="two"; Integer = 2; Float = 2.0}) 
    Assert.Equal(5, output.Count) // one for the root, one for each member (plus mystery "Item")

[<Fact>]
let ``walker shouldn't compare the same union case with different fancy cases properly`` () =
    let output = ResizeArray<string>()
    let walker (e1: obj) (e2: obj) (name: string list) =        
        output.Add(sprintf "%A %A %A" e1 e2 name)
        
    let settings = { WalkerSettings.Default with CompareCSharpProperties = true }
    let walkerFun = generateWalker<TestUnion2> settings walker
    walkerFun (EmbedRec {String ="one"; Integer = 1; Float = 1.0}) (EmbedFields("two", 2, 2.0))
    Assert.Equal(1, output.Count) // one for the root

    output.Clear()
    walkerFun (EmbedFields("two", 2, 2.0)) (EmbedRec {String ="one"; Integer = 1; Float = 1.0})
    Assert.Equal(1, output.Count) // one for the root
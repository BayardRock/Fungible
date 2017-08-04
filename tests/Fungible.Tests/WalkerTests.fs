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
    member t.Intger = Integer
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

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
        
    let walkerFun = generateWalker<TestPOCO> WalkerSettings.Default walker
    walkerFun (TestPOCO("Str1",1,1.0)) (TestPOCO("Str2",2,2.0))
    Assert.Equal(4, output.Count)

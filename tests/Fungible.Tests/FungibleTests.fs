module Fungible.Tests

open Fungible

open NUnit.Framework

open System
open System.Collections.Generic

open Microsoft.FSharp.Quotations

type SimpleRecord = { Name: string; Age: int }
type LotsOfRecords = { People: SimpleRecord [] }

let makeMap (f: 'a -> 'a) = <@@ f @@> |> Map
let makeFilter (f: 'a -> bool) = <@@ f @@> |> Filter
let makeCollect (f: 'a -> 'a []) = <@@ f @@> |> Collect
let makeDefault (f: unit -> 'a) = <@@ f @@> |> Default
let makeFunction (f: 'a -> 'a) = <@@ f @@> |> Function
let makeAdder (f: unit -> 'a) = <@@ f @@> |> Add

let rcs = RecordCloningSettings.Default
//
// Simple Types 
//

[<Test>]
let ``record cloning should be able to clone a simple record`` () =
    let sr = { Name = "Rick"; Age = 33 }
    let func = Fungible.genrateRecordDeepCopyFunction<SimpleRecord> rcs Map.empty
    let res = func sr
    Assert.AreEqual(sr, res)

[<Test>]
let ``record cloning should be able to map a simple string`` () =
    let sr = { Name = "Rick"; Age = 33 }
    let updater (s: string) = s.ToLower()
    let map = [(["Name"], [makeMap updater])] |> Map.ofSeq
    let func = Fungible.genrateRecordDeepCopyFunction<SimpleRecord> rcs map
    let res = func sr
    Assert.True((res = { sr with Name = "rick" }), sprintf "Failed with: %A" res)

[<Test>]
let ``record cloning should be able to double map a simple string in the right order`` () =
    let sr = { Name = "Rick"; Age = 33 }
    let toLower (s: string) = s.ToLower()
    let addI (s: string) = s + "I"
    let map = [(["Name"], [makeMap addI; makeMap toLower])] |> Map.ofSeq
    let func = Fungible.genrateRecordDeepCopyFunction<SimpleRecord> rcs map
    let res = func sr
    Assert.True((res = { sr with Name = "ricki" }), sprintf "Failed with: %A" res)

[<Test>]
let ``record cloning should not be able to filter a simple string`` () =
    let sr = { Name = "Rick"; Age = 33 }
    let updater (s: string) = s = ""
    let map = [(["Name"], [makeFilter updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> Fungible.genrateRecordDeepCopyFunction<SimpleRecord> rcs map |> ignore) |> ignore

[<Test>]
let ``record cloning should not be able to collect a simple string`` () =
    let sr = { Name = "Rick"; Age = 33 }
    let updater (s: string) = s.Split('i')
    let map = [(["Name"], [makeCollect updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> Fungible.genrateRecordDeepCopyFunction<SimpleRecord> rcs map |> ignore) |> ignore

[<Test>]
let ``record cloning should not be able to apply a function to a simple string`` () =
    let sr = { Name = "Rick"; Age = 33 }
    let updater (s: string) = s.ToLower()
    let map = [(["Name"], [makeFunction updater])] |> Map.ofSeq
    let func = Fungible.genrateRecordDeepCopyFunction<SimpleRecord> rcs map
    let res = func sr
    Assert.True((res = { sr with Name = "rick" }), sprintf "Failed with: %A" res)

//
// Arrays with Simple Types
//

type SimpleArrayRecord = { Names: string array }

[<Test>]
let ``record cloning should be able to clone a record with a simple array`` () =
    let sr = { Names = [|"Rick"; "David"; "Mark"; "Paul"; "Pete"|] }
    let func = Fungible.genrateRecordDeepCopyFunction<SimpleArrayRecord> rcs Map.empty
    let res = func sr
    Assert.AreEqual(sr, res)

[<Test>]
let ``record cloning should be able to map over a record with a simple array`` () =
    let sr = { Names = [|"Rick"; "David"; "Mark"; "Paul"; "Pete"|] }
    let updater (s: string) = s.ToLower()
    let map = [(["Names"], [makeMap updater])] |> Map.ofSeq
    let func = Fungible.genrateRecordDeepCopyFunction<SimpleArrayRecord> rcs map
    let res = func sr
    let expected = { sr with Names = sr.Names |> Array.map (fun s -> s.ToLowerInvariant()) }
    Assert.AreEqual(expected, res)

[<Test>]
let ``record cloning should be able to filter a record with a simple array`` () =
    let sr = { Names = [|"Rick"; "David"; "Mark"; "Paul"; "Pete"|] }
    let filterFun cs = cs <> "Rick"
    let map = [(["Names"], [makeFilter filterFun])] |> Map.ofSeq
    let func = Fungible.genrateRecordDeepCopyFunction<SimpleArrayRecord> rcs map
    let res = func sr
    let expected = { sr with Names = sr.Names.[1..] }
    Assert.AreEqual(expected, res)

[<Test>]
let ``record cloning should be able to collect a record with a simple array`` () =
    let sr = { Names = [|"Rick;David"; "Mark;Paul"; "Pete"|] }

    let collectFun (s: string) = s.Split([|';'|], StringSplitOptions.None)
    let map = [(["Names"], [makeCollect collectFun])] |> Map.ofSeq
    let func = Fungible.genrateRecordDeepCopyFunction<SimpleArrayRecord> rcs map
    let res = func sr
    let expected = { Names = [|"Rick"; "David"; "Mark"; "Paul"; "Pete"|] }
    Assert.AreEqual(expected, res)

[<Test>]
let ``record cloning should be able to apply a function to a record with a simple array`` () =
    let sr = { Names = [|"Rick"; "David"; "Mark"; "Paul"; "Pete"|] }
    let updater (s: string []) = s |> Array.append [| "Guy" |]
    let map = [(["Names"], [makeFunction updater])] |> Map.ofSeq
    let func = Fungible.genrateRecordDeepCopyFunction<SimpleArrayRecord> rcs map
    let res = func sr
    let expected = { sr with Names = sr.Names |> Array.append [| "Guy" |] }
    Assert.AreEqual(expected, res)

[<Test>]
let ``record cloning should be able to map filter and collect a record with a simple array in the right order`` () =
    let sr = { Names = [|"Rick;David"; "Mark;Paul"; "Pete"|] }

    let collectFun (s: string) = s.Split([|';'|], StringSplitOptions.None)
    let filterFun cs = cs <> "Rick"
    let addFun () = [|"Guy"|]
    let mapFun (s: string) = s.ToUpper()

    let map = [(["Names"], [makeCollect collectFun; makeFilter filterFun; makeAdder addFun; makeMap mapFun])] |> Map.ofSeq
    let func = Fungible.genrateRecordDeepCopyFunction<SimpleArrayRecord> rcs map
    let res = func sr
    let expected = { Names = [|"DAVID"; "MARK"; "PAUL"; "PETE"; "GUY"|] }
    Assert.AreEqual(expected, res)


[<Test>]
let ``record cloning should be able to add to a simple array type`` () =
    let sr = { Names = [|"Rick"; "David" |] }    
    let addFun () = [|"Mark"; "Pete"|]
    let map = [(["Names"], [makeAdder addFun])] |> Map.ofSeq
    let func = Fungible.genrateRecordDeepCopyFunction<SimpleArrayRecord> rcs map
    let res = func sr
    let expected = { Names = [|"Rick"; "David"; "Mark"; "Pete"|] }
    Assert.AreEqual(expected, res)

//
// Arrays of Records
//

[<Test>]
let ``record cloning should be able to clone a record with an array of records`` () =
    let sr = { People = [|{Name = "Rick"; Age = 33 }; { Name = "Paul"; Age = 55 }|] }
    let func = Fungible.genrateRecordDeepCopyFunction<LotsOfRecords> rcs Map.empty
    let res = func sr
    Assert.AreEqual(sr, res)

[<Test>]
let ``record cloning should be able to map over a record with an array of records`` () =
    let sr = { People = [|{Name = "Rick"; Age = 33 }; { Name = "Paul"; Age = 55 }|] }
    let updater (c: string) = c.ToLower() 
    let map = [(["Name"; "People"], [makeMap updater])] |> Map.ofSeq
    let func = Fungible.genrateRecordDeepCopyFunction<LotsOfRecords> rcs map
    let res = func sr
    let expected = {sr with People = sr.People |> Array.map (fun r -> { r with Name = r.Name.ToLowerInvariant() }) }
    Assert.AreEqual(expected, res)

[<Test>]
let ``record cloning should not be able to filter over a record with an array of records`` () =
    let sr = { People = [|{Name = "Rick"; Age = 33 }; { Name = "Paul"; Age = 55 }|] }
    let updater (c: string) = c <> "Rick" 
    let map = [(["Name"; "People"], [makeFilter updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> Fungible.genrateRecordDeepCopyFunction<LotsOfRecords> rcs map |> ignore) |> ignore

[<Test>]
let ``record cloning should not be able to collect over a record with an array of records`` () =
    let sr = { People = [|{Name = "Rick"; Age = 33 }; { Name = "Paul"; Age = 55 }|] }
    let updater (c: string) = c.Split('i')
    let map = [(["Name"; "People"], [makeCollect updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> Fungible.genrateRecordDeepCopyFunction<LotsOfRecords> rcs map |> ignore) |> ignore

[<Test>]
let ``record cloning should be able to combine array and record level operations`` () =
    let sr = { People = [|{Name = "Rick"; Age = 33 }; { Name = "Paul"; Age = 55 }|] }
    let mapfun (c: string) = c.ToLower() 
    let filterfun (c: SimpleRecord) = c.Name = "paul"
    let map = [["Name"; "People"], [makeMap mapfun]
               ["People"], [makeFilter filterfun]
              ] |> Map.ofSeq
    let func = Fungible.genrateRecordDeepCopyFunction<LotsOfRecords> rcs map
    let res = func sr
    let expected = {sr with People = sr.People 
                                     |> Array.map (fun r -> { r with Name = r.Name.ToLowerInvariant() }) 
                                     |> Array.filter (fun r -> r.Name = "paul")
                   }
    Assert.AreEqual(expected, res)

//
// Option types
//

type ThatGuy = { Name: string; Friend: SimpleRecord option; Ages: int array}
with static member Default =  { Name = "Rick"; Friend = None; Ages = [||] }

[<Test>]
let ``record cloning should be able to clone a record with an option type`` () =
    let sr1 = { ThatGuy.Default with Friend = Some <| { Name = "Mark"; Age = -10 }}
    let sr2 = ThatGuy.Default
    let func = Fungible.genrateRecordDeepCopyFunction<ThatGuy> rcs Map.empty
    let res = func sr1 in
        Assert.AreEqual(sr1, res)
    let res = func sr2 in
        Assert.AreEqual(sr2, res)

[<Test>]
let ``record cloning should be able to map a record with an option type`` () =
    let sr1 = { ThatGuy.Default with Friend = Some <| { Name = "Mark"; Age = -10 }}
    let sr2 = ThatGuy.Default

    let updater (c: string) = c.ToLower() 
    let map = [(["Name"; "Friend"], [makeMap updater])] |> Map.ofSeq

    let func = Fungible.genrateRecordDeepCopyFunction<ThatGuy> rcs map
    let res = func sr1 in 
        let expected = { sr1 with Friend = Some <| { sr1.Friend.Value with Name = sr1.Friend.Value.Name.ToLowerInvariant() } }
        Assert.AreEqual(expected, res)
    let res = func sr2 in Assert.AreEqual(sr2, res) // No Change

[<Test>]
let ``record cloning should be able to filter a record with an option type`` () =
    let sr1 = { ThatGuy.Default with Friend = Some <| { Name = "Mark"; Age = -10 }}
    let sr2 = ThatGuy.Default

    let updater (c: SimpleRecord) = c.Name <> "Mark"
    let map = [(["Friend"], [makeFilter updater])] |> Map.ofSeq

    let func = Fungible.genrateRecordDeepCopyFunction<ThatGuy> rcs map
    let res = func sr1 in 
        let expected = { sr1 with Friend = None }
        Assert.AreEqual(expected, res)
    let res = func sr2 in Assert.AreEqual(sr2, res) // No Change

[<Test>]
let ``record cloning should be able to map and filter a record with an option type in the correct order`` () =
    let sr1 = { ThatGuy.Default with Friend = Some <| { Name = "Mark"; Age = -10 }}
    let sr2 = ThatGuy.Default

    let mapFun (c: string) = c.ToUpper()
    let filterFun (c: SimpleRecord) = c.Name <> "MARK"

    let map = 
        [["Name"; "Friend"], [makeMap mapFun]
         ["Friend"], [makeFilter filterFun]
        ] |> Map.ofSeq

    let func = Fungible.genrateRecordDeepCopyFunction<ThatGuy> rcs map
    let res = func sr1 in 
        let expected = { sr1 with Friend = None }
        Assert.AreEqual(expected, res)
    let res = func sr2 in Assert.AreEqual(sr2, res) // No Change

[<Test>]
let ``record cloning should not be able to collect a record with an option type`` () =
    let sr1 = { ThatGuy.Default with Friend = Some <| { Name = "Mark"; Age = -10 }}
    let sr2 = ThatGuy.Default

    let updater (c: string) = c.Split([|'k'|])
    let map = [(["Name"; "Friend"], [makeCollect updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> Fungible.genrateRecordDeepCopyFunction<ThatGuy> rcs map |> ignore) |> ignore

type SimpleOptionRec = { Place: string option }

[<Test>]
let ``record cloning should be able to apply a function to an option type`` () = 
    let sr1 = { Place = Some "Here" }

    let updater (c: string option) = c |> Option.map (fun v -> v.ToLower()) 
    let map = [(["Place"], [makeFunction updater])] |> Map.ofSeq

    let func = Fungible.genrateRecordDeepCopyFunction<SimpleOptionRec> rcs map
    let res = func sr1 in 
        let expected = { Place = Some "here" }
        Assert.AreEqual(expected, res)


//
// Union Types
//

type Bird = | Loud of int | Quiet 
type BirdOwner = { Person: SimpleRecord; BirdType: Bird }

[<Test>]
let ``record cloning should be able to clone a record with a union type`` () =
    let sr1 = { Person = { Name = "Rick"; Age = 33 }; BirdType = Loud 1000 }
    let func = Fungible.genrateRecordDeepCopyFunction<BirdOwner> rcs Map.empty
    let res = func sr1 in
        Assert.AreEqual(sr1, res)

[<Test>]
let ``record cloning should be able to map a record with a union type`` () =
    let sr1 = { Person = { Name = "Rick"; Age = 33 }; BirdType = Loud 1000 }
    let sr2 = { Person = { Name = "Rick"; Age = 33 }; BirdType = Quiet }

    let updater l = l - 10
    let map = [(["BirdType"], [makeMap updater])] |> Map.ofSeq

    let func = Fungible.genrateRecordDeepCopyFunction<BirdOwner> rcs map
    let res = func sr1 in
        let expected = { sr1 with BirdType = Loud 990 }
        Assert.AreEqual(expected, res)
    let res = func sr2 in 
        Assert.AreEqual(sr2, res)

    
[<Test>]
let ``record cloning should not be able to filter a record with a union type`` () =
    let sr1 = { Person = { Name = "Rick"; Age = 33 }; BirdType = Loud 1000 }

    let updater l = l < 0
    let map = [(["BirdType"], [makeFilter updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> Fungible.genrateRecordDeepCopyFunction<BirdOwner> rcs map |> ignore) |> ignore

[<Test>]
let ``record cloning should not be able to collect a record with a union type`` () =
    let sr1 = { Person = { Name = "Rick"; Age = 33 }; BirdType = Loud 1000 }

    let updater l = [| l - 1; l + 1 |]
    let map = [(["BirdType"], [makeCollect updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> Fungible.genrateRecordDeepCopyFunction<BirdOwner> rcs map |> ignore) |> ignore
    
type MaybeName = { Name: string option }
[<Test>]
let ``record cloning should be able to default a simple option type`` () =
    let sr = { Name = None }
    let updater () = Some "Rick"
    let defaulter = [(["Name"], [makeDefault updater])] |> Map.ofSeq

    let func = Fungible.genrateRecordDeepCopyFunction<MaybeName> rcs defaulter
    let res = func sr in
        let expected = { sr with Name = Some "Rick" }
        Assert.AreEqual(expected, res)

type ArrayName = { Name: string [] }
[<Test>]
let ``record cloning should be able to default a simple array type`` () =
    let sr = { Name = [||] }
    let updater () = [| "Rick"; "Mark" |]
    let defaulter = [(["Name"], [makeDefault updater])] |> Map.ofSeq

    let func = Fungible.genrateRecordDeepCopyFunction<ArrayName> rcs defaulter
    let res = func sr in
        let expected = { sr with Name = [| "Rick"; "Mark" |] }
        Assert.AreEqual(expected, res)

type MapNamePlace = { Lookup: Map<string, string> }

[<Test>]
let ``record cloning should be able to clone a record with a map in it`` () =
    let sr0 = { Lookup = Map.empty }
    let sr1 = { Lookup = ["Dude", "Place"] |> Map.ofList }
    let func = Fungible.genrateRecordDeepCopyFunction<MapNamePlace> rcs Map.empty
    let res = func sr0 in
        Assert.AreEqual(sr0, res)
    let res = func sr1 in
        Assert.AreEqual(sr1, res)

[<Test>]
let ``record cloning should be able map over a map`` () =
    let sr0 = { Lookup = Map.empty }
    let sr1 = { Lookup = ["Dude", "Place"] |> Map.ofList }
    let updater (k : string, v : string) = k, v.ToLower() 
    let map = [(["Lookup"], [ <@@ updater @@> |> Map ])] |> Map.ofSeq

    let func = Fungible.genrateRecordDeepCopyFunction<MapNamePlace> rcs map
    let res = func sr0 in
        Assert.AreEqual(sr0, { Lookup = Map.empty })
    let res = func sr1 in
        Assert.AreEqual(res, { Lookup = ["Dude", "place"] |> Map.ofList })

[<Test>]
let ``record cloning should be able filter over a map`` () =
    let sr0 = { Lookup = Map.empty }
    let sr1 = { Lookup = ["Dude", "Place"; "Rick", "ESB" ] |> Map.ofList }

    let filter (k,v : string) = k = "Dude" 
    let map = [(["Lookup"], [ <@@ filter @@> |> Filter ])] |> Map.ofSeq

    let func = Fungible.genrateRecordDeepCopyFunction<MapNamePlace> rcs map
    let res = func sr0 in
        Assert.AreEqual(sr0, { Lookup = Map.empty })
    let res = func sr1 in
        Assert.AreEqual(res, { Lookup = ["Dude", "Place"] |> Map.ofList })

[<Test>]
let ``record cloning should be able default a map`` () =
    let sr0 = { Lookup = Map.empty }
    let sr1 = { Lookup = ["Dude", "Place"; "Rick", "ESB" ] |> Map.ofList }

    let deffun () = ["Default", "Map"] |> Map.ofList

    let map = [(["Lookup"], [ <@@ deffun @@> |> Default ])] |> Map.ofSeq

    let func = Fungible.genrateRecordDeepCopyFunction<MapNamePlace> rcs map
    let res0 = func sr0 
    Assert.AreEqual(res0, { sr0 with Lookup = ["Default", "Map"] |> Map.ofList })
    let res1 = func sr1 
    Assert.AreEqual(res1, sr1)

[<Test>]
let ``record cloning should be able add to a map`` () =
    let sr0 = { Lookup = Map.empty }
    let sr1 = { Lookup = ["Dude", "Place"; "Rick", "ESB" ] |> Map.ofList }

    let addfun () = ["Default", "Map"] |> Map.ofList

    let map = [(["Lookup"], [ <@@ addfun @@> |> Add ])] |> Map.ofSeq

    let func = Fungible.genrateRecordDeepCopyFunction<MapNamePlace> rcs map
    let res0 = func sr0 
    Assert.AreEqual(res0, { sr0 with Lookup = ["Default", "Map"] |> Map.ofList })
    let res1 = func sr1 
    Assert.AreEqual(res1, { sr1 with Lookup = ["Dude", "Place"; "Rick", "ESB"; "Default", "Map" ] |> Map.ofList })

//
// Helpers
//

[<Test>]
let ``path builder should build a reasonable path for a simple type`` () =
    let expected = 
        [
            ["Name"], typeof<string>
            ["Friend"], typeof<SimpleRecord option>
            ["Name"; "Friend"], typeof<string>
            ["Age"; "Friend"], typeof<int>
            ["Ages"], typeof<int array>
        ] |> List.map (fun (p,t) -> p, t.ToString()) |> Map.ofList
    let paths = getPathsAndTypes<ThatGuy>() |> Map.map (fun k v -> v.ToString())

    Assert.AreEqual(expected, paths)

    
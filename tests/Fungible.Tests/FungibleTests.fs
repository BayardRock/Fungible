module Fungible.Tests

open Fungible.Core

open NUnit.Framework

open System
open System.Collections.Generic

open Microsoft.FSharp.Quotations

type SimpleRecord = { Name: string; Age: int }

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

let simpleTest = { Name = "Rick"; Age = 33 }

[<Test>]
let ``record cloning should be able to clone a simple record`` () =
    let func = genrateRecordTransformFunction<SimpleRecord> rcs Map.empty
    let res = func simpleTest
    Assert.AreEqual(simpleTest, res)

[<Test>]
let ``record cloning should be able to map a simple string`` () =
    let updater (s: string) = s.ToLower()
    let map = [(["Name"], [makeMap updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<SimpleRecord> rcs map
    let res = func simpleTest
    Assert.True((res = { simpleTest with Name = "rick" }), sprintf "Failed with: %A" res)

[<Test>]
let ``record cloning should be able to double map a simple string in the right order`` () =
    let toLower (s: string) = s.ToLower()
    let addI (s: string) = s + "I"
    let map = [(["Name"], [makeMap addI; makeMap toLower])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<SimpleRecord> rcs map
    let res = func simpleTest
    Assert.True((res = { simpleTest with Name = "ricki" }), sprintf "Failed with: %A" res)

[<Test>]
let ``record cloning should not be able to filter a simple string`` () =
    let updater (s: string) = s = ""
    let map = [(["Name"], [makeFilter updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> genrateRecordTransformFunction<SimpleRecord> rcs map |> ignore) |> ignore

[<Test>]
let ``record cloning should not be able to collect a simple string`` () =
    let updater (s: string) = s.Split('i')
    let map = [(["Name"], [makeCollect updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> genrateRecordTransformFunction<SimpleRecord> rcs map |> ignore) |> ignore

[<Test>]
let ``record cloning should not be able to apply a function to a simple string`` () =
    let updater (s: string) = s.ToLower()
    let map = [(["Name"], [makeFunction updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<SimpleRecord> rcs map
    let res = func simpleTest
    Assert.True((res = { simpleTest with Name = "rick" }), sprintf "Failed with: %A" res)

//
// Arrays with Simple Types
//

type SimpleArrayRecord = { Names: string array }
let simpleArrayRecordEx = { Names = [|"Rick"; "David"; "Mark"; "Paul"; "Pete"|] }

[<Test>]
let ``record cloning should be able to clone a record with a simple array`` () =
    let func = genrateRecordTransformFunction<SimpleArrayRecord> rcs Map.empty
    let res = func simpleArrayRecordEx
    Assert.AreEqual(simpleArrayRecordEx, res)

[<Test>]
let ``record cloning should be able to map over a record with a simple array`` () =
    let updater (s: string) = s.ToLower()
    let map = [(["Names"], [makeMap updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<SimpleArrayRecord> rcs map
    let res = func simpleArrayRecordEx
    let expected = { simpleArrayRecordEx with Names = simpleArrayRecordEx.Names |> Array.map (fun s -> s.ToLowerInvariant()) }
    Assert.AreEqual(expected, res)

[<Test>]
let ``record cloning should be able to filter a record with a simple array`` () =
    let filterFun cs = cs <> "Rick"
    let map = [(["Names"], [makeFilter filterFun])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<SimpleArrayRecord> rcs map
    let res = func simpleArrayRecordEx
    let expected = { simpleArrayRecordEx with Names = simpleArrayRecordEx.Names.[1..] }
    Assert.AreEqual(expected, res)

[<Test>]
let ``record cloning should be able to collect a record with a simple array`` () =
    let sr = { Names = [|"Rick;David"; "Mark;Paul"; "Pete"|] }
    let collectFun (s: string) = s.Split([|';'|], StringSplitOptions.None)

    let map = [(["Names"], [makeCollect collectFun])] |> Map.ofSeq
    let func = genrateRecordTransformFunction<SimpleArrayRecord> rcs map
    let res = func sr
    let expected = { Names = [|"Rick"; "David"; "Mark"; "Paul"; "Pete"|] }
    Assert.AreEqual(expected, res)

[<Test>]
let ``record cloning should be able to apply a function to a record with a simple array`` () =
    let updater (s: string []) = s |> Array.append [| "Guy" |]
    let map = [(["Names"], [makeFunction updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<SimpleArrayRecord> rcs map
    let res = func simpleArrayRecordEx
    let expected = { simpleArrayRecordEx with Names = simpleArrayRecordEx.Names |> Array.append [| "Guy" |] }
    Assert.AreEqual(expected, res)

[<Test>]
let ``record cloning should be able to map filter and collect a record with a simple array in the right order`` () =
    let sr = { Names = [|"Rick;David"; "Mark;Paul"; "Pete"|] }

    let collectFun (s: string) = s.Split([|';'|], StringSplitOptions.None)
    let filterFun cs = cs <> "Rick"
    let addFun () = [|"Guy"|]
    let mapFun (s: string) = s.ToUpper()

    let map = [(["Names"], [makeCollect collectFun; makeFilter filterFun; makeAdder addFun; makeMap mapFun])] |> Map.ofSeq
    let func = genrateRecordTransformFunction<SimpleArrayRecord> rcs map
    let res = func sr
    let expected = { Names = [|"DAVID"; "MARK"; "PAUL"; "PETE"; "GUY"|] }
    Assert.AreEqual(expected, res)


[<Test>]
let ``record cloning should be able to add to a simple array type`` () =
    let sr = { Names = [|"Rick"; "David" |] }    
    let addFun () = [|"Mark"; "Pete"|]

    let map = [(["Names"], [makeAdder addFun])] |> Map.ofSeq
    let func = genrateRecordTransformFunction<SimpleArrayRecord> rcs map
    let res = func sr
    let expected = { Names = [|"Rick"; "David"; "Mark"; "Pete"|] }
    Assert.AreEqual(expected, res)

//
// Arrays of Records
//

type LotsOfRecords = { People: SimpleRecord [] }
let lotsOfRecordsEx = { People = [|{Name = "Rick"; Age = 33 }; { Name = "Paul"; Age = 55 }|] }

[<Test>]
let ``record cloning should be able to clone a record with an array of records`` () =
    let func = genrateRecordTransformFunction<LotsOfRecords> rcs Map.empty
    let res = func lotsOfRecordsEx
    Assert.AreEqual(lotsOfRecordsEx, res)

[<Test>]
let ``record cloning should be able to map over a record with an array of records`` () =
    let updater (c: string) = c.ToLower() 
    let map = [(["Name"; "People"], [makeMap updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<LotsOfRecords> rcs map
    let res = func lotsOfRecordsEx
    let expected = {lotsOfRecordsEx with People = lotsOfRecordsEx.People |> Array.map (fun r -> { r with Name = r.Name.ToLowerInvariant() }) }
    Assert.AreEqual(expected, res)

[<Test>]
let ``record cloning should not be able to filter over a record with an array of records`` () =
    let updater (c: string) = c <> "Rick" 
    let map = [(["Name"; "People"], [makeFilter updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> genrateRecordTransformFunction<LotsOfRecords> rcs map |> ignore) |> ignore

[<Test>]
let ``record cloning should not be able to collect over a record with an array of records`` () =
    let updater (c: string) = c.Split('i')
    let map = [(["Name"; "People"], [makeCollect updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> genrateRecordTransformFunction<LotsOfRecords> rcs map |> ignore) |> ignore

[<Test>]
let ``record cloning should be able to combine array and record level operations`` () =
    let mapfun (c: string) = c.ToLower() 
    let filterfun (c: SimpleRecord) = c.Name = "paul"
    let map = [["Name"; "People"], [makeMap mapfun]
               ["People"], [makeFilter filterfun]
              ] |> Map.ofSeq

    let func = genrateRecordTransformFunction<LotsOfRecords> rcs map
    let res = func lotsOfRecordsEx
    let expected = {lotsOfRecordsEx with People = lotsOfRecordsEx.People 
                                                  |> Array.map (fun r -> { r with Name = r.Name.ToLowerInvariant() }) 
                                                  |> Array.filter (fun r -> r.Name = "paul")
                   }
    Assert.AreEqual(expected, res)

//
// Option types
//

type ThatGuy = { Name: string; Friend: SimpleRecord option; Ages: int array}

let thatGuyEx = { Name = "Rick"; Friend = None; Ages = [||] }
let thatGuyWithFriendEx = { thatGuyEx with Friend = Some <| { Name = "Mark"; Age = -10 }}

[<Test>]
let ``record cloning should be able to clone a record with an option type`` () =
    let func = genrateRecordTransformFunction<ThatGuy> rcs Map.empty
    let res = func thatGuyWithFriendEx in
        Assert.AreEqual(thatGuyWithFriendEx, res)
    let res = func thatGuyEx in
        Assert.AreEqual(thatGuyEx, res)

[<Test>]
let ``record cloning should be able to map a record with an option type`` () =
    let updater (c: string) = c.ToLower() 
    let map = [(["Name"; "Friend"], [makeMap updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<ThatGuy> rcs map
    let res = func thatGuyWithFriendEx in 
        let expected = { thatGuyWithFriendEx with Friend = Some <| { thatGuyWithFriendEx.Friend.Value with Name = thatGuyWithFriendEx.Friend.Value.Name.ToLowerInvariant() } }
        Assert.AreEqual(expected, res)
    let res = func thatGuyEx in Assert.AreEqual(thatGuyEx, res) // No Change

[<Test>]
let ``record cloning should be able to filter a record with an option type`` () =
    let updater (c: SimpleRecord) = c.Name <> "Mark"
    let map = [(["Friend"], [makeFilter updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<ThatGuy> rcs map
    let res = func thatGuyWithFriendEx in 
        let expected = { thatGuyWithFriendEx with Friend = None }
        Assert.AreEqual(expected, res)
    let res = func thatGuyEx in Assert.AreEqual(thatGuyEx, res) // No Change

[<Test>]
let ``record cloning should be able to map and filter a record with an option type in the correct order`` () =
    let mapFun (c: string) = c.ToUpper()
    let filterFun (c: SimpleRecord) = c.Name <> "MARK"

    let map = 
        [["Name"; "Friend"], [makeMap mapFun]
         ["Friend"], [makeFilter filterFun]
        ] |> Map.ofSeq

    let func = genrateRecordTransformFunction<ThatGuy> rcs map
    let res = func thatGuyWithFriendEx in 
        let expected = { thatGuyWithFriendEx with Friend = None }
        Assert.AreEqual(expected, res)
    let res = func thatGuyEx in Assert.AreEqual(thatGuyEx, res) // No Change

[<Test>]
let ``record cloning should not be able to collect a record with an option type`` () =
    let updater (c: string) = c.Split([|'k'|])
    let map = [(["Name"; "Friend"], [makeCollect updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> genrateRecordTransformFunction<ThatGuy> rcs map |> ignore) |> ignore

type SimpleOptionRec = { Place: string option }
let simpleOptionRecEx = { Place = Some "Here" }

[<Test>]
let ``record cloning should be able to apply a function to an option type`` () = 
    let updater (c: string option) = c |> Option.map (fun v -> v.ToLower()) 
    let map = [(["Place"], [makeFunction updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<SimpleOptionRec> rcs map
    let res = func simpleOptionRecEx in 
        let expected = { Place = Some "here" }
        Assert.AreEqual(expected, res)


//
// Union Types
//

type Bird = | Loud of int | Quiet 
type BirdOwner = { Person: SimpleRecord; BirdType: Bird }

let quietBirdOwnerEx = { Person = { Name = "Rick"; Age = 33 }; BirdType = Quiet }
let loudBirdOwnerEx = { Person = { Name = "Rick"; Age = 33 }; BirdType = Loud 1000 }

[<Test>]
let ``record cloning should be able to clone a record with a union type`` () =
    let func = genrateRecordTransformFunction<BirdOwner> rcs Map.empty
    let res = func loudBirdOwnerEx in
        Assert.AreEqual(loudBirdOwnerEx, res)

[<Test>]
let ``record cloning should be able to map a record with a union type`` () =
    let updater l = l - 10
    let map = [(["BirdType"], [makeMap updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<BirdOwner> rcs map
    let res = func loudBirdOwnerEx in
        let expected = { loudBirdOwnerEx with BirdType = Loud 990 }
        Assert.AreEqual(expected, res)
    let res = func quietBirdOwnerEx in 
        Assert.AreEqual(quietBirdOwnerEx, res)

    
[<Test>]
let ``record cloning should not be able to filter a record with a union type`` () =
    let updater l = l < 0
    let map = [(["BirdType"], [makeFilter updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> genrateRecordTransformFunction<BirdOwner> rcs map |> ignore) |> ignore

[<Test>]
let ``record cloning should not be able to collect a record with a union type`` () =
    let updater l = [| l - 1; l + 1 |]
    let map = [(["BirdType"], [makeCollect updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> genrateRecordTransformFunction<BirdOwner> rcs map |> ignore) |> ignore
    
type MaybeName = { Name: string option }
let maybeNameNoneEx = { Name = None }

[<Test>]
let ``record cloning should be able to default a simple option type`` () =
    let updater () = Some "Rick"
    let defaulter = [(["Name"], [makeDefault updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<MaybeName> rcs defaulter
    let res = func maybeNameNoneEx in
        let expected = { maybeNameNoneEx with Name = Some "Rick" }
        Assert.AreEqual(expected, res)

type ArrayName = { Name: string [] }
let arrayNameEmptyEx =  { Name = [||] }

[<Test>]
let ``record cloning should be able to default a simple array type`` () =
    let updater () = [| "Rick"; "Mark" |]
    let defaulter = [(["Name"], [makeDefault updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<ArrayName> rcs defaulter
    let res = func arrayNameEmptyEx in
        let expected = { arrayNameEmptyEx with Name = [| "Rick"; "Mark" |] }
        Assert.AreEqual(expected, res)

type MapNamePlace = { Lookup: Map<string, string> }
let mapNamePlaceEmptyEx = { Lookup = Map.empty }
let mapNamePlaceEx = { Lookup = ["Dude", "Place"] |> Map.ofList }
let mapNamePlaceLongEx = { Lookup = ["Dude", "Place"; "Rick", "ESB" ] |> Map.ofList }

[<Test>]
let ``record cloning should be able to clone a record with a map in it`` () =
    let func = genrateRecordTransformFunction<MapNamePlace> rcs Map.empty
    let res = func mapNamePlaceEmptyEx in
        Assert.AreEqual(mapNamePlaceEmptyEx, res)
    let res = func mapNamePlaceEx in
        Assert.AreEqual(mapNamePlaceEx, res)

[<Test>]
let ``record cloning should be able map over a map`` () =
    let updater (k : string, v : string) = k, v.ToLower() 
    let map = [(["Lookup"], [ <@@ updater @@> |> Map ])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<MapNamePlace> rcs map
    let res = func mapNamePlaceEmptyEx in
        Assert.AreEqual(mapNamePlaceEmptyEx, { Lookup = Map.empty })
    let res = func mapNamePlaceEx in
        Assert.AreEqual(res, { Lookup = ["Dude", "place"] |> Map.ofList })

[<Test>]
let ``record cloning should be able filter over a map`` () =
    let sr0 = { Lookup = Map.empty }
    let sr1 = { Lookup = ["Dude", "Place"; "Rick", "ESB" ] |> Map.ofList }

    let filter (k,v : string) = k = "Dude" 
    let map = [(["Lookup"], [ <@@ filter @@> |> Filter ])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<MapNamePlace> rcs map
    let res = func sr0 in
        Assert.AreEqual(sr0, { Lookup = Map.empty })
    let res = func sr1 in
        Assert.AreEqual(res, { Lookup = ["Dude", "Place"] |> Map.ofList })

[<Test>]
let ``record cloning should be able default a map`` () =
    let deffun () = ["Default", "Map"] |> Map.ofList

    let map = [(["Lookup"], [ <@@ deffun @@> |> Default ])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<MapNamePlace> rcs map
    let res0 = func mapNamePlaceEmptyEx 
    Assert.AreEqual(res0, { mapNamePlaceEmptyEx with Lookup = ["Default", "Map"] |> Map.ofList })
    let res1 = func mapNamePlaceLongEx 
    Assert.AreEqual(res1, mapNamePlaceLongEx)

[<Test>]
let ``record cloning should be able add to a map`` () =
    let addfun () = ["Default", "Map"] |> Map.ofList

    let map = [(["Lookup"], [ <@@ addfun @@> |> Add ])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<MapNamePlace> rcs map
    let res0 = func mapNamePlaceEmptyEx 
    Assert.AreEqual(res0, { mapNamePlaceEmptyEx with Lookup = ["Default", "Map"] |> Map.ofList })
    let res1 = func mapNamePlaceLongEx 
    Assert.AreEqual(res1, { mapNamePlaceLongEx with Lookup = ["Dude", "Place"; "Rick", "ESB"; "Default", "Map" ] |> Map.ofList })

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

    
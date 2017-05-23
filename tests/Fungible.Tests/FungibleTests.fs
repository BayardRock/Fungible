module Fungible.Tests

open Fungible.Core

open Xunit

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
let makeComparer (f: (string list * 'a * 'a) -> unit) = <@@ f @@> |> Compare

let rcs = FungibleCoreSettings.Default
//
// Simple Types 
//

let simpleTest = { Name = "Rick"; Age = 33 }

[<Fact>]
let ``record cloning should be able to clone a simple record`` () =
    let func = genrateRecordTransformFunction<SimpleRecord> rcs Map.empty
    let res = func simpleTest
    Assert.Equal(simpleTest, res)

[<Fact>]
let ``record cloning should be able to map a simple string`` () =
    let updater (s: string) = s.ToLower()
    let map = [(["Name"], [makeMap updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<SimpleRecord> rcs map
    let res = func simpleTest
    Assert.True((res = { simpleTest with Name = "rick" }), sprintf "Failed with: %A" res)

[<Fact>]
let ``record cloning should be able to double map a simple string in the right order`` () =
    let toLower (s: string) = s.ToLower()
    let addI (s: string) = s + "I"
    let map = [(["Name"], [makeMap addI; makeMap toLower])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<SimpleRecord> rcs map
    let res = func simpleTest
    Assert.True((res = { simpleTest with Name = "ricki" }), sprintf "Failed with: %A" res)

[<Fact>]
let ``record cloning should not be able to filter a simple string`` () =
    let updater (s: string) = s = ""
    let map = [(["Name"], [makeFilter updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> genrateRecordTransformFunction<SimpleRecord> rcs map |> ignore) |> ignore

[<Fact>]
let ``record cloning should not be able to collect a simple string`` () =
    let updater (s: string) = s.Split('i')
    let map = [(["Name"], [makeCollect updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> genrateRecordTransformFunction<SimpleRecord> rcs map |> ignore) |> ignore

[<Fact>]
let ``record cloning should not be able to apply a function to a simple string`` () =
    let updater (s: string) = s.ToLower()
    let map = [(["Name"], [makeFunction updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<SimpleRecord> rcs map
    let res = func simpleTest
    Assert.True((res = { simpleTest with Name = "rick" }), sprintf "Failed with: %A" res)

[<Fact>]
let ``recording cloning should be able to report a change to a simple string`` () = 
    let updater (s: string) = s.ToLower() 
    let mutable compResult : bool Option = None
    let mutable compResultName: string Option = None
    let comparer (s: string list, v1: string, v2: string) = compResult <- Some (v1 = v2); compResultName <- (s |> String.concat "." |> Some)
    let map = [(["Name"], [makeFunction updater; makeComparer comparer])] |> Map.ofSeq
   
    let func = genrateRecordTransformFunction<SimpleRecord> rcs map
    let res = func simpleTest
    Assert.True((res = { simpleTest with Name = "rick" }), sprintf "Failed with: %A" res)
    Assert.True((compResult = Some false))
    Assert.True((compResultName = Some "Name"))

//
// Arrays with Simple Types
//

type SimpleArrayRecord = { Names: string array }
let simpleArrayRecordEx = { Names = [|"Rick"; "David"; "Mark"; "Paul"; "Pete"|] }

[<Fact>]
let ``record cloning should be able to clone a record with a simple array`` () =
    let func = genrateRecordTransformFunction<SimpleArrayRecord> rcs Map.empty
    let res = func simpleArrayRecordEx
    Assert.Equal(simpleArrayRecordEx, res)

[<Fact>]
let ``record cloning should be able to map over a record with a simple array`` () =
    let updater (s: string) = s.ToLower()
    let map = [(["Names"], [makeMap updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<SimpleArrayRecord> rcs map
    let res = func simpleArrayRecordEx
    let expected = { simpleArrayRecordEx with Names = simpleArrayRecordEx.Names |> Array.map (fun s -> s.ToLowerInvariant()) }
    Assert.Equal(expected, res)

[<Fact>]
let ``record cloning should be able to filter a record with a simple array`` () =
    let filterFun cs = cs <> "Rick"
    let map = [(["Names"], [makeFilter filterFun])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<SimpleArrayRecord> rcs map
    let res = func simpleArrayRecordEx
    let expected = { simpleArrayRecordEx with Names = simpleArrayRecordEx.Names.[1..] }
    Assert.Equal(expected, res)

[<Fact>]
let ``record cloning should be able to collect a record with a simple array`` () =
    let sr = { Names = [|"Rick;David"; "Mark;Paul"; "Pete"|] }
    let collectFun (s: string) = s.Split([|';'|], StringSplitOptions.None)

    let map = [(["Names"], [makeCollect collectFun])] |> Map.ofSeq
    let func = genrateRecordTransformFunction<SimpleArrayRecord> rcs map
    let res = func sr
    let expected = { Names = [|"Rick"; "David"; "Mark"; "Paul"; "Pete"|] }
    Assert.Equal(expected, res)

[<Fact>]
let ``record cloning should be able to apply a function to a record with a simple array`` () =
    let updater (s: string []) = s |> Array.append [| "Guy" |]
    let map = [(["Names"], [makeFunction updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<SimpleArrayRecord> rcs map
    let res = func simpleArrayRecordEx
    let expected = { simpleArrayRecordEx with Names = simpleArrayRecordEx.Names |> Array.append [| "Guy" |] }
    Assert.Equal(expected, res)

[<Fact>]
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
    Assert.Equal(expected, res)


[<Fact>]
let ``record cloning should be able to add to a simple array type`` () =
    let sr = { Names = [|"Rick"; "David" |] }    
    let addFun () = [|"Mark"; "Pete"|]

    let map = [(["Names"], [makeAdder addFun])] |> Map.ofSeq
    let func = genrateRecordTransformFunction<SimpleArrayRecord> rcs map
    let res = func sr
    let expected = { Names = [|"Rick"; "David"; "Mark"; "Pete"|] }
    Assert.Equal(expected, res)

[<Fact>]
let ``record cloning should be able to compare a simple array type`` () = 
    let sr = { Names = [|"Rick"; "David" |] }    
    let addFun () = [|"Mark"; "Pete"|]
    let mutable compResult : bool Option = None
    let mutable compResultName: string Option = None
    let compFun (s: string list, v1: string [], v2: string []) = compResult <- Some (v1 = v2); compResultName <- (s |> String.concat "." |> Some)

    let map = [(["Names"], [makeAdder addFun; makeComparer compFun])] |> Map.ofSeq    

    let func = genrateRecordTransformFunction<SimpleArrayRecord> rcs map
    let res = func sr
    let expected = { Names = [|"Rick"; "David"; "Mark"; "Pete"|] }

    Assert.Equal(expected, res)
    Assert.True((compResult = Some false))
    Assert.True((compResultName = Some "Names"))

//
// Arrays of Records
//

type LotsOfRecords = { People: SimpleRecord [] }
let lotsOfRecordsEx = { People = [|{Name = "Rick"; Age = 33 }; { Name = "Paul"; Age = 55 }|] }

[<Fact>]
let ``record cloning should be able to clone a record with an array of records`` () =
    let func = genrateRecordTransformFunction<LotsOfRecords> rcs Map.empty
    let res = func lotsOfRecordsEx
    Assert.Equal(lotsOfRecordsEx, res)

[<Fact>]
let ``record cloning should be able to map over a record with an array of records`` () =
    let updater (c: string) = c.ToLower() 
    let map = [(["Name"; "People"], [makeMap updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<LotsOfRecords> rcs map
    let res = func lotsOfRecordsEx
    let expected = {lotsOfRecordsEx with People = lotsOfRecordsEx.People |> Array.map (fun r -> { r with Name = r.Name.ToLowerInvariant() }) }
    Assert.Equal(expected, res)

[<Fact>]
let ``record cloning should be able to compare over a record with an array of records`` () = 
    let updater (c: string) = c.ToLower() 
    let mutable compResult : bool Option = None
    let mutable compResultName: string Option = None
    let compFun (s: string list, v1: string, v2: string) = compResult <- Some (v1 = v2); compResultName <- (s |> String.concat "." |> Some)
    let map = [(["Name"; "People"], [makeMap updater; makeComparer compFun])] |> Map.ofSeq  

    let func = genrateRecordTransformFunction<LotsOfRecords> rcs map
    let res = func lotsOfRecordsEx
    let expected = {lotsOfRecordsEx with People = lotsOfRecordsEx.People |> Array.map (fun r -> { r with Name = r.Name.ToLowerInvariant() }) }

    Assert.Equal(expected, res)
    Assert.True((compResult = Some false))
    match compResultName with
    | Some resName -> Assert.Equal(resName, "People.Name")
    | None -> Assert.False(false)

[<Fact>]
let ``record cloning should not be able to filter over a record with an array of records`` () =
    let updater (c: string) = c <> "Rick" 
    let map = [(["Name"; "People"], [makeFilter updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> genrateRecordTransformFunction<LotsOfRecords> rcs map |> ignore) |> ignore

[<Fact>]
let ``record cloning should not be able to collect over a record with an array of records`` () =
    let updater (c: string) = c.Split('i')
    let map = [(["Name"; "People"], [makeCollect updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> genrateRecordTransformFunction<LotsOfRecords> rcs map |> ignore) |> ignore

[<Fact>]
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
    Assert.Equal(expected, res)

//
// Option types
//

type ThatGuy = { Name: string; Friend: SimpleRecord option; Ages: int array}

let thatGuyEx = { Name = "Rick"; Friend = None; Ages = [||] }
let thatGuyWithFriendEx = { thatGuyEx with Friend = Some <| { Name = "Mark"; Age = -10 }}

type OptionGuy = { Name: string option }
let thatOptionGuy = { Name = Some "Rick" }

[<Fact>]
let ``record cloning should be able to clone a record with an option type`` () =
    let func = genrateRecordTransformFunction<ThatGuy> rcs Map.empty
    let res = func thatGuyWithFriendEx in
        Assert.Equal(thatGuyWithFriendEx, res)
    let res = func thatGuyEx in
        Assert.Equal(thatGuyEx, res)

[<Fact>]
let ``record cloning should be able to map a record with an option type`` () =
    let updater (c: string) = c.ToLower() 
    let map = [(["Name"; "Friend"], [makeMap updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<ThatGuy> rcs map
    let res = func thatGuyWithFriendEx in 
        let expected = { thatGuyWithFriendEx with Friend = Some <| { thatGuyWithFriendEx.Friend.Value with Name = thatGuyWithFriendEx.Friend.Value.Name.ToLowerInvariant() } }
        Assert.Equal(expected, res)
    let res = func thatGuyEx in Assert.Equal(thatGuyEx, res) // No Change

[<Fact>]
let ``record cloning should be able to filter a record with an option type`` () =
    let updater (c: SimpleRecord) = c.Name <> "Mark"
    let map = [(["Friend"], [makeFilter updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<ThatGuy> rcs map
    let res = func thatGuyWithFriendEx in 
        let expected = { thatGuyWithFriendEx with Friend = None }
        Assert.Equal(expected, res)
    let res = func thatGuyEx in Assert.Equal(thatGuyEx, res) // No Change

[<Fact>]
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
        Assert.Equal(expected, res)
    let res = func thatGuyEx in Assert.Equal(thatGuyEx, res) // No Change

[<Fact>]
let ``record cloning should not be able to collect a record with an option type`` () =
    let updater (c: string) = c.Split([|'k'|])
    let map = [(["Name"; "Friend"], [makeCollect updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> genrateRecordTransformFunction<ThatGuy> rcs map |> ignore) |> ignore

type SimpleOptionRec = { Place: string option }
let simpleOptionRecEx = { Place = Some "Here" }

[<Fact>]
let ``record cloning should be able to apply a function to an option type`` () = 
    let updater (c: string option) = c |> Option.map (fun v -> v.ToLower()) 
    let map = [(["Place"], [makeFunction updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<SimpleOptionRec> rcs map
    let res = func simpleOptionRecEx in 
        let expected = { Place = Some "here" }
        Assert.Equal(expected, res)

[<Fact>]
let ``record cloning should be able to compare an option type`` () = 
    let updater (c: string) = c.ToLower() 
    let mutable compResult : bool Option = None
    let mutable compResultName: string Option = None
    let compFun (s: string list, v1: string option, v2: string option) = compResult <- Some (v1 = v2); compResultName <- (s |> String.concat "." |> Some)
    let map = [(["Name"], [makeMap updater; makeComparer compFun])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<OptionGuy> rcs map
    let res = func thatOptionGuy in 
        let expected = { thatOptionGuy with Name = Some "rick" }
        Assert.Equal(expected, res)

    Assert.True((compResult = Some false))
    match compResultName with
    | Some resName -> Assert.Equal(resName, "Name")
    | None -> Assert.False(false)

[<Fact>]
let ``record cloning should be able to compare over a nested option type`` () = 
    let updater (c: string) = c.ToLower() 
    let mutable compResult : bool Option = None
    let mutable compResultName: string Option = None
    let compFun (s: string list, v1: string, v2: string) = compResult <- Some (v1 = v2); compResultName <- (s |> String.concat "." |> Some)
    let map = [(["Name"; "Friend"], [makeMap updater; makeComparer compFun])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<ThatGuy> rcs map
    let res = func thatGuyWithFriendEx in 
        let expected = { thatGuyWithFriendEx with Friend = Some <| { thatGuyWithFriendEx.Friend.Value with Name = thatGuyWithFriendEx.Friend.Value.Name.ToLowerInvariant() } }
        Assert.Equal(expected, res)
    let res = func thatGuyEx in Assert.Equal(thatGuyEx, res) // No Change

    Assert.True((compResult = Some false))
    match compResultName with
    | Some resName -> Assert.Equal(resName, "Friend.Name")
    | None -> Assert.False(false)

//
// Union Types
//

type Bird = | Loud of int | Quiet 
type BirdOwner = { Person: SimpleRecord; BirdType: Bird }

let quietBirdOwnerEx = { Person = { Name = "Rick"; Age = 33 }; BirdType = Quiet }
let loudBirdOwnerEx = { Person = { Name = "Rick"; Age = 33 }; BirdType = Loud 1000 }

[<Fact>]
let ``record cloning should be able to clone a record with a union type`` () =
    let func = genrateRecordTransformFunction<BirdOwner> rcs Map.empty
    let res = func loudBirdOwnerEx in
        Assert.Equal(loudBirdOwnerEx, res)

[<Fact>]
let ``record cloning should be able to map a record with a union type`` () =
    let updater l = l - 10
    let map = [(["BirdType"], [makeMap updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<BirdOwner> rcs map
    let res = func loudBirdOwnerEx in
        let expected = { loudBirdOwnerEx with BirdType = Loud 990 }
        Assert.Equal(expected, res)
    let res = func quietBirdOwnerEx in 
        Assert.Equal(quietBirdOwnerEx, res)

[<Fact(Skip="Unable to perform functions that exactly target a union type")>]
let ``record cloning should be able to compare a record with a union type`` () =
    let updater l = l - 10
    let mutable compResult : bool Option = None
    let mutable compResultName: string Option = None
    let compFun (s: string list, v1: Bird, v2: Bird) = compResult <- Some (v1 = v2); compResultName <- (s |> String.concat "." |> Some)
    let map = [(["BirdType"], [makeMap updater; makeComparer compFun])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<BirdOwner> rcs map
    let res = func loudBirdOwnerEx in
        let expected = { loudBirdOwnerEx with BirdType = Loud 990 }
        Assert.Equal(expected, res)
    let res = func quietBirdOwnerEx in 
        Assert.Equal(quietBirdOwnerEx, res)

    Assert.True((compResult = Some false))
    match compResultName with
    | Some resName -> Assert.Equal(resName, "BirdType")
    | None -> Assert.False(false)
    
[<Fact>]
let ``record cloning should not be able to filter a record with a union type`` () =
    let updater l = l < 0
    let map = [(["BirdType"], [makeFilter updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> genrateRecordTransformFunction<BirdOwner> rcs map |> ignore) |> ignore

[<Fact>]
let ``record cloning should not be able to collect a record with a union type`` () =
    let updater l = [| l - 1; l + 1 |]
    let map = [(["BirdType"], [makeCollect updater])] |> Map.ofSeq
    Assert.Throws<Exception> (fun () -> genrateRecordTransformFunction<BirdOwner> rcs map |> ignore) |> ignore
    
type MaybeName = { Name: string option }
let maybeNameNoneEx = { Name = None }

[<Fact>]
let ``record cloning should be able to default a simple option type`` () =
    let updater () = Some "Rick"
    let defaulter = [(["Name"], [makeDefault updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<MaybeName> rcs defaulter
    let res = func maybeNameNoneEx in
        let expected = { maybeNameNoneEx with Name = Some "Rick" }
        Assert.Equal(expected, res)

type ArrayName = { Name: string [] }
let arrayNameEmptyEx =  { Name = [||] }

[<Fact>]
let ``record cloning should be able to default a simple array type`` () =
    let updater () = [| "Rick"; "Mark" |]
    let defaulter = [(["Name"], [makeDefault updater])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<ArrayName> rcs defaulter
    let res = func arrayNameEmptyEx in
        let expected = { arrayNameEmptyEx with Name = [| "Rick"; "Mark" |] }
        Assert.Equal(expected, res)

type MapNamePlace = { Lookup: Map<string, string> }
let mapNamePlaceEmptyEx = { Lookup = Map.empty }
let mapNamePlaceEx = { Lookup = ["Dude", "Place"] |> Map.ofList }
let mapNamePlaceLongEx = { Lookup = ["Dude", "Place"; "Rick", "ESB" ] |> Map.ofList }

[<Fact>]
let ``record cloning should be able to clone a record with a map in it`` () =
    let func = genrateRecordTransformFunction<MapNamePlace> rcs Map.empty
    let res = func mapNamePlaceEmptyEx in
        Assert.Equal(mapNamePlaceEmptyEx, res)
    let res = func mapNamePlaceEx in
        Assert.Equal(mapNamePlaceEx, res)

[<Fact>]
let ``record cloning should be able map over a map`` () =
    let updater (k : string, v : string) = k, v.ToLower() 
    let map = [(["Lookup"], [ <@@ updater @@> |> Map ])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<MapNamePlace> rcs map
    let res = func mapNamePlaceEmptyEx in
        Assert.Equal(mapNamePlaceEmptyEx, { Lookup = Map.empty })
    let res = func mapNamePlaceEx in
        Assert.Equal(res, { Lookup = ["Dude", "place"] |> Map.ofList })

[<Fact>]
let ``record cloning should be able filter over a map`` () =
    let sr0 = { Lookup = Map.empty }
    let sr1 = { Lookup = ["Dude", "Place"; "Rick", "ESB" ] |> Map.ofList }

    let filter (k,v : string) = k = "Dude" 
    let map = [(["Lookup"], [ <@@ filter @@> |> Filter ])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<MapNamePlace> rcs map
    let res = func sr0 in
        Assert.Equal(sr0, { Lookup = Map.empty })
    let res = func sr1 in
        Assert.Equal(res, { Lookup = ["Dude", "Place"] |> Map.ofList })

[<Fact>]
let ``record cloning should be able default a map`` () =
    let deffun () = ["Default", "Map"] |> Map.ofList

    let map = [(["Lookup"], [ <@@ deffun @@> |> Default ])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<MapNamePlace> rcs map
    let res0 = func mapNamePlaceEmptyEx 
    Assert.Equal(res0, { mapNamePlaceEmptyEx with Lookup = ["Default", "Map"] |> Map.ofList })
    let res1 = func mapNamePlaceLongEx 
    Assert.Equal(res1, mapNamePlaceLongEx)

[<Fact>]
let ``record cloning should be able add to a map`` () =
    let addfun () = ["Default", "Map"] |> Map.ofList

    let map = [(["Lookup"], [ <@@ addfun @@> |> Add ])] |> Map.ofSeq

    let func = genrateRecordTransformFunction<MapNamePlace> rcs map
    let res0 = func mapNamePlaceEmptyEx 
    Assert.Equal(res0, { mapNamePlaceEmptyEx with Lookup = ["Default", "Map"] |> Map.ofList })
    let res1 = func mapNamePlaceLongEx 
    Assert.Equal(res1, { mapNamePlaceLongEx with Lookup = ["Dude", "Place"; "Rick", "ESB"; "Default", "Map" ] |> Map.ofList })



//
// Helpers
//

[<Fact>]
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

    Assert.True((expected = paths))

    
module SimpleFunctions.Tests

open Fungible.Core
open Fungible.StaticReflection

open System
open System.Text
open System.Globalization
open System.ComponentModel

open Xunit


type Location = 
    {
        City: string option
        State: string option
        Country: string option
    } 
    static member Default = { City = None; State = None; Country = None }

type Person =
    {
        Names: string []
        StructuredNames: string [] []
        Locations: Location []
        IsImportant: bool
    } static member Default = { Names = [||]; StructuredNames = [||]; Locations = [||]; IsImportant = false}

type Hints = 
    {
        Class: string
    } static member Default = { Class = "Person" }

type Pair = 
    {
        Cust: Person
        Prov: Person
        Hints: Hints
    } static member Default = { Cust = Person.Default; Prov = Person.Default; Hints = Hints.Default }

let testRec = 
    {
        Pair.Default with
            Cust =  { Person.Default with IsImportant = false; Locations = [| { Location.Default with City = Some "West Hartford"; Country = Some "USA" } |] }
            Prov = { Person.Default with IsImportant = false; Names = [|"The Dude"|]; Locations = [| { Location.Default with City = Some "Hartford"; State = Some "CT" } |] }
            Hints = { Hints.Default with Class = "Individual" }
    }

module TestFunctions = 
    type internal Marker = interface end
    let ModuleType = typeof<Marker>.DeclaringType

    [<TransformFunctionTypeAttribute("Map")>]
    [<Description("Replaces accented characters with their english equivalents")>]
    let stripAccents (inStr: string) = 
        let sb2 = new StringBuilder()
        for c in inStr.Normalize(NormalizationForm.FormD) do    
            let cat = CharUnicodeInfo.GetUnicodeCategory(c)
            if cat <> UnicodeCategory.NonSpacingMark && cat <> UnicodeCategory.SpacingCombiningMark && cat <> UnicodeCategory.EnclosingMark then
                sb2.Append(c) |> ignore
        sb2.ToString().Normalize(NormalizationForm.FormC)    

    [<TransformFunctionTypeAttribute("Map")>]
    [<Description("Removes the specified characters from the given string")>]
    let removeCharacters (toRemove: char []) (inStr: string) =    
        let sb = new StringBuilder()
        for c in inStr do
            if Array.IndexOf(toRemove, c) = -1 then sb.Append(c) |> ignore
        sb.ToString()   


    [<TransformFunctionTypeAttribute("Function")>]
    [<Description("Adds the following countries with none are specified")>]
    let defaultCountries (countries: string []) (locs: Location []) = 
        if locs |> Seq.forall (fun l -> l.Country.IsNone) then
            locs |> Array.append [| for c in countries do yield { Location.Default with Country = Some c } |]
        else locs

let cleanEngineHitBasic<'T> (cleaners: TransformDefinition []) eh = 
    let propertyMap = getPathsAndTypes<'T>()
    let basicCleaners = cleaners |> Array.map (generateTransform<'T> TestFunctions.ModuleType propertyMap)
    let cleanerFunc = compileTransforms<'T, 'T> basicCleaners
    cleanerFunc eh eh

[<Fact>]
let ``basic data cleaning should properly work for removing extended chars from names`` () = 
    let testRec = { testRec with Cust = { testRec.Cust with Names = [| "Montréal" |] } } 
    let settings =  [| { TargetPath = "Cust.Names"; FunctionName = "stripAccents"; FunctionArgs = [||] } |]
    let res = cleanEngineHitBasic settings testRec
    Assert.Equal<string []>([| "Montreal" |], res.Cust.Names)

[<Fact>]
let ``basic data cleaning should properly work for mapping nested arrays of names`` () = 
    let testRec = { Person.Default with StructuredNames = [|[|"Montréal"|]; [|"Montréal"|]|]}

    let settings =  [| { TargetPath = "StructuredNames"; FunctionName = "stripAccents"; FunctionArgs = [||] } |]
    let res = cleanEngineHitBasic settings testRec
    Assert.Equal<string []>([|[| "Montreal" |]; [| "Montreal" |]|], res.StructuredNames)

[<Fact>]
let ``basic data cleaning should properly map over option types nested in records in arrays`` () = 
    let testRec = { testRec with Cust = { testRec.Cust with Locations = [| { Location.Default with City = Some "Montréal" } |] } } 
    let settings =  [| { TargetPath = "Cust.Locations.City"; FunctionName = "stripAccents"; FunctionArgs = [||] } |]
    let res = cleanEngineHitBasic settings testRec
    Assert.Equal("Montreal", res.Cust.Locations.[0].City.Value)

[<Fact>]
let ``basic data cleaning should properly work for replacing arbitrary substrings`` () = 
    let testRec = { testRec with Cust = { testRec.Cust with Names = [| "Montréal" |] } } 
    let settings =  [| { TargetPath = "Cust.Names"; FunctionName = "removeCharacters"; FunctionArgs = [|"M"; "o"; "n"; "t"|] } |]
    let res = cleanEngineHitBasic settings testRec
    Assert.Equal<string []>([| "réal" |], res.Cust.Names)

[<Fact>]
let ``basic data cleaning should properly default arrays of objects`` () =
    let testRec = { testRec with Cust = { testRec.Cust with Locations = [||] } } 
    let settings =  [| { TargetPath = "Cust.Locations"; FunctionName = "defaultCountries"; FunctionArgs = [|"USA"; "US"|] } |]
    let res = cleanEngineHitBasic settings testRec
    Assert.Equal<Location []>([| { Location.Default with Country = Some "USA" }; { Location.Default with Country = Some "US" } |], res.Cust.Locations)
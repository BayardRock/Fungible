(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/Fungible"

(**
Introducing your project
========================

Let's start with the core functionality by loading the basic references 
and opening the appropriate namespaces.

*)
#r "Fungible.dll"

open Fungible
open Fungible.Core

(**
Next we need to define a record tree structure to operate on.
*)

type Date = { Day: byte; Month: byte; Year: uint16 }
type Location = { Address: string option; City: string option; 
                  State: string option; Country: string option }
type Person = { Names: string []; DOBs: Date []; 
                Citizenships: string []; Locations: Location []; 
                Fields: Map<string, string> }

(**
As well as a sample to play around with.
*)

let testPersons =
    [
        { Names = [| "Rick Minerich" |]; DOBs = [| { Day = 8uy; Month = 24uy; Year = 1886us } |]
          Citizenships = [| "Mars"; "Twitter" |]
          Locations = [| { Address = None; City = Some "New York"; State = Some "NY"; Country = None } |]
          Fields = Map.empty
        }
    ] 

(**
For a simple starting example we will just transform the City field within 
the Locations member of person to be uppercase. When the type is None no action 
will be taken, when it's Some the transform will be applied.
*)
let upperFunMapExpr = 
    let upperFun (s: string) = s.ToUpper()
    <@@ upperFun @@> |> Map

let ex1 = 
    let functionMap = [(["City"; "Locations"], [ upperFunMapExpr ])] |> Map.ofList
    let transform = genrateRecordTransformFunction<Person> (FungibleCoreSettings.Default) functionMap
    testPersons |> List.map transform

let ex2 = 
    let functionMap = [(["Address"; "Locations"], [ upperFunMapExpr ])] |> Map.ofList
    let transform = genrateRecordTransformFunction<Person> (FungibleCoreSettings.Default) functionMap
    testPersons |> List.map transform


(**
But what if you want to uppercase many different fields? Just add a path 
for each place you want to change with the corresponding function.
*)

let ex3 =
    let functionMap = [ ["Names"], [upperFunMapExpr] 
                        ["Citizenships"], [upperFunMapExpr] 
                        ["Address"; "Locations"], [upperFunMapExpr] 
                        ["City"; "Locations"], [upperFunMapExpr] 
                        ["State"; "Locations"], [upperFunMapExpr] 
                        ["Country"; "Locations"], [upperFunMapExpr] 
                      ] |> Map.ofList
    let transform = genrateRecordTransformFunction<Person> (FungibleCoreSettings.Default) functionMap
    testPersons |> List.map transform

(**
What if we want to apply multiple different transforms to the same field? Just list the transforms
and they'll be executed in order.
*)

let removeUpperVowelsExpr = 
    let removeUpperVowels (s: string) =
        let vowels = [|'A'; 'E'; 'I'; 'O'; 'U'|] // TODO: Sometimes Y?
        s |> String.filter (fun c -> Array.contains c vowels |> not)
    <@@ removeUpperVowels @@> |> Map

let ex4 = 
    let functionMap = [(["City"; "Locations"], [ upperFunMapExpr; removeUpperVowelsExpr ])] |> Map.ofList
    let transform = genrateRecordTransformFunction<Person> (FungibleCoreSettings.Default) functionMap
    testPersons |> List.map transform

let ex5 = 
    let functionMap = [(["City"; "Locations"], [ removeUpperVowelsExpr; upperFunMapExpr ])] |> Map.ofList
    let transform = genrateRecordTransformFunction<Person> (FungibleCoreSettings.Default) functionMap
    testPersons |> List.map transform

(**
Let's dig into the static reflection part of the library, which is used
for pulling functions in from F# libraries based on a settings spec.
*)

open Fungible.StaticReflection

module TestFunctions = 
    open System
    open System.Text
    open System.ComponentModel

    type internal Marker = interface end
    let ModuleType = typeof<Marker>.DeclaringType

    [<TransformFunctionType("Map")>]
    [<Description("Removes the specified characters from the given string")>]
    let removeCharacters (toRemove: char []) (inStr: string) =    
        let sb = new StringBuilder()
        for c in inStr do            
            if Array.IndexOf(toRemove, c) = -1 then sb.Append(c) |> ignore
        sb.ToString()   

let ex6 = 
    let settings =  [| { TargetPath = "Names"; 
                         FunctionName = "removeCharacters"; 
                         FunctionArgs = [| "R"; "r" |] } |]
    let propertyMap = getPathsAndTypes<Person>()
    let generator = generateTransform TestFunctions.ModuleType propertyMap
    let transforms = settings |> Array.map generator
    let compiledTransforms = compileTransforms<Person, Person> transforms
    testPersons |> List.map (fun p -> compiledTransforms p p)


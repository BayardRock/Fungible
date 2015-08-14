namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Fungible.Barb")>]
[<assembly: AssemblyProductAttribute("Fungible")>]
[<assembly: AssemblyDescriptionAttribute("A library for fast reflective changes to immutable data trees")>]
[<assembly: AssemblyVersionAttribute("0.0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.1"

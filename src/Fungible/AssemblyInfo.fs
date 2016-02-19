namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Fungible")>]
[<assembly: AssemblyProductAttribute("Fungible")>]
[<assembly: AssemblyDescriptionAttribute("A library for fast reflective changes to immutable data trees")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"

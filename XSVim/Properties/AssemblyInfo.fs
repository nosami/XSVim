namespace XSVim
open System.Reflection
open System.Runtime.CompilerServices

[<AutoOpen>]
module AddinVersion =
    [<Literal>]
    let version = "0.49.4"

[<assembly: AssemblyTitle("XSVim")>]
[<assembly: AssemblyDescription("")>]
[<assembly: AssemblyConfiguration("")>]
[<assembly: AssemblyCompany("")>]
[<assembly: AssemblyProduct("")>]
[<assembly: AssemblyCopyright("${AuthorCopyright}")>]
[<assembly: AssemblyTrademark("")>]

// The assembly version has the format {Major}.{Minor}.{Build}.{Revision}

[<assembly: AssemblyVersion(version)>]

//[<assembly: AssemblyDelaySign(false)>]
//[<assembly: AssemblyKeyFile("")>]

()

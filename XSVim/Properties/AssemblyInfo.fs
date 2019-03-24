namespace XSVim
open System.Reflection
open System.Runtime.CompilerServices

[<AutoOpen>]
module AddinVersion =
    [<Literal>]
    let version = "0.65.0.80"

[<assembly: AssemblyTitle("XSVim")>]
// The assembly version has the format {Major}.{Minor}.{Build}.{VSMacVersion}

[<assembly: AssemblyVersion(version)>]

//[<assembly: AssemblyDelaySign(false)>]
//[<assembly: AssemblyKeyFile("")>]

()

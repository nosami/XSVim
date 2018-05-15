﻿namespace XSVim
open System.Reflection
open System.Runtime.CompilerServices

[<AutoOpen>]
module AddinVersion =
    [<Literal>]
    let version = "0.59.0"

[<assembly: AssemblyTitle("XSVim")>]
// The assembly version has the format {Major}.{Minor}.{Build}.{Revision}

[<assembly: AssemblyVersion(version)>]

//[<assembly: AssemblyDelaySign(false)>]
//[<assembly: AssemblyKeyFile("")>]

()

namespace XSVim.Tests

open System
open Mono.Addins
open Mono.Addins.Description

[<assembly:Addin (
  "XSVim.Tests", 
  Namespace = "XSVim.Tests",
  Version = "1.0"
)>]

[<assembly:AddinName ("XSVim.Tests")>]
[<assembly:AddinCategory ("IDE extensions")>]
[<assembly:AddinDescription ("XSVim.Tests")>]
[<assembly:AddinAuthor ("jason")>]
()

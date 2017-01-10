namespace XSVim

open System
open Mono.Addins
open Mono.Addins.Description

[<assembly:Addin (
  "XSVim", 
  Namespace = "XSVim",
  Version = "0.9"
)>]

[<assembly:AddinName ("Vim")>]
[<assembly:AddinCategory ("IDE extensions")>]
[<assembly:AddinDescription ("Vim emulation layer for Xamarin Studio / Visual Studio for Mac.")>]
[<assembly:AddinUrl ("https://github.com/nosami/XSVim")>]
[<assembly:AddinAuthor ("jason")>]
()

namespace XSVim

open Mono.Addins
open MonoDevelop
[<assembly:Addin (
  "XSVim",
  Namespace = "XSVim",
  Version = version
)>]

[<assembly:AddinName ("Vim")>]
[<assembly:AddinCategory ("IDE extensions")>]
[<assembly:AddinDescription ("Vim emulation layer for Xamarin Studio / Visual Studio for Mac.")>]
[<assembly:AddinUrl ("https://github.com/nosami/XSVim")>]
[<assembly:AddinAuthor ("jason")>]
[<assembly:AddinDependency ("::MonoDevelop.Core", "8.1")>]
[<assembly:AddinDependency ("::MonoDevelop.Ide", "8.1")>]
[<assembly:AddinDependency ("::MonoDevelop.SourceEditor2", "8.1")>]
()

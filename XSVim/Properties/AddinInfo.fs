namespace XSVim

open Mono.Addins
open MonoDevelop
[<assembly:Addin (
  "XSVim",
  Namespace = "XSVim",
  Version = "0.49.2"
)>]

[<assembly:AddinName ("Vim")>]
[<assembly:AddinCategory ("IDE extensions")>]
[<assembly:AddinDescription ("Vim emulation layer for Xamarin Studio / Visual Studio for Mac.")>]
[<assembly:AddinUrl ("https://github.com/nosami/XSVim")>]
[<assembly:AddinAuthor ("jason")>]
[<assembly:AddinDependency ("::MonoDevelop.Core", BuildInfo.Version)>]
[<assembly:AddinDependency ("::MonoDevelop.Ide", BuildInfo.Version)>]
[<assembly:AddinDependency ("::MonoDevelop.SourceEditor2", BuildInfo.Version)>]
()

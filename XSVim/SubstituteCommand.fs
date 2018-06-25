namespace XSVim
open System
open MonoDevelop.Core
open MonoDevelop.Core.ProgressMonitoring
open MonoDevelop.Ide
open MonoDevelop.Ide.FindInFiles

type SubstituteScope = Selection | Document

type Substitution = { find: string; replace: string; scope: SubstituteScope } 

module Substitute =
  let substitute substitution =
    let find = FindInFiles.FindReplace()
    let options = FindInFiles.FilterOptions()
    options.RegexSearch <- true
    if not(find.ValidatePattern(options, substitution.find)) then
        MessageService.ShowError (GettextCatalog.GetString ("Search pattern is invalid"));
        false
    elif not(find.ValidatePattern(options, substitution.replace)) then
        MessageService.ShowError (GettextCatalog.GetString ("Replace pattern is invalid"));
        false
    else
        use monitor =
            match IdeApp.Workbench with
            | null -> new ConsoleProgressMonitor() :> ProgressMonitor
            | workbench -> 
                let monitor = workbench.ProgressMonitors.GetSearchProgressMonitor (true)
                monitor.PathMode <- PathMode.Hidden
                monitor :> ProgressMonitor
        let scope =
            match substitution.scope with
            | Document -> DocumentScope() :> Scope
            | Selection -> SelectionScope() :> Scope

        find.FindAll(scope, monitor, substitution.find, substitution.replace, options, Threading.CancellationToken.None)
        |> Seq.iter(fun res ->
            match monitor with
            | :? SearchProgressMonitor as mon ->
                mon.ReportResult res
            | _ -> printfn "%A" res)

        true
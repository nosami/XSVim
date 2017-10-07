namespace XSVim
open System
open MonoDevelop.Ide
open MonoDevelop.Ide.Commands
open MonoDevelop.Ide.Editor.Extension

module exMode =
    let getFirstCharAndRest (s:string) = s.[0], s.[1..]

    let processCommand command =
        let firstChar, rest = getFirstCharAndRest command
        match firstChar, rest with
        | '/', _ -> [ runOnce (IncrementalSearch rest) Nothing ]
        | '?', _ -> [ runOnce (IncrementalSearchBackwards rest) Nothing ]
        | _ -> wait

    let save() = IdeApp.Workbench.ActiveDocument.Save() |> Async.AwaitTask
    let forceClose() = IdeApp.Workbench.ActiveDocument.Window.CloseWindow true |> Async.AwaitTask |> Async.Ignore

    let processKey (state:VimState) (key:KeyDescriptor) =
        let setMessage message = { state with statusMessage = message }
        let normalMode = { state with statusMessage = None; mode = NormalMode }
        match key.SpecialKey with
        | SpecialKey.BackSpace ->
            let message =
                match state.statusMessage with
                | Some msg ->
                    let len = msg.Length
                    msg.[0..len-2]
                | None -> ""
            if message.Length > 0 then
                setMessage (Some message), processCommand message
            else
                normalMode, resetKeys
        | SpecialKey.Return ->
            match state.statusMessage with
            | Some message ->
                let firstChar, rest = getFirstCharAndRest message
                let getSearchAction() = match state.searchAction with | Some action -> action | _ -> Move
                let restIsNumeric, number = Int32.TryParse rest
                // really bad parser. TODO: try and use https://github.com/jaredpar/VsVim/blob/447d980da9aa6c761238e39df9d2b64424643de1/Src/VimCore/Interpreter_Parser.fs
                match firstChar, rest with
                | '/', rest ->
                    { state with statusMessage = None; mode = NormalMode; lastSearch = Some (Jump (ToSearch rest)) }
                    , [ runOnce (getSearchAction()) (Jump (ToSearch rest))]
                | '?', rest ->
                    { state with statusMessage = None; mode = NormalMode; lastSearch = Some (Jump (ToSearchBackwards rest)) }
                    , [ runOnce (getSearchAction()) (Jump (ToSearchBackwards rest))]
                | ':', _rest when restIsNumeric ->
                    { state with statusMessage = None; mode = NormalMode; }
                    , [ runOnce Move (Jump (StartOfLineNumber number)) ]
                | ':', "q"  ->
                    async {
                        dispatchCommand FileCommands.CloseFile
                    } |> Async.StartImmediate
                    normalMode, resetKeys
                | ':', "q!"  ->
                    async {
                        do! forceClose()
                    } |> Async.StartImmediate
                    normalMode, resetKeys
                | ':', "w"
                | ':', "w!"  ->
                    async {
                        do! save()
                    } |> Async.StartImmediate
                    normalMode, resetKeys
                | ':', "wa"
                | ':', "wa!"  ->
                    async {
                        dispatchCommand FileCommands.SaveAll
                    } |> Async.StartImmediate
                    normalMode, resetKeys
                | ':', "qa"  ->
                    async {
                        dispatchCommand FileCommands.CloseAllFiles
                    } |> Async.StartImmediate
                    normalMode, resetKeys
                | ':', "qa!"  ->
                    IdeApp.Workbench.Documents
                    |> Seq.iter(fun doc ->
                        async {
                            do! doc.Window.CloseWindow true |> Async.AwaitTask |> Async.Ignore
                        } |> Async.StartImmediate)
                    normalMode, resetKeys
                | ':', "wq"  ->
                    async {
                        do! save()
                        dispatchCommand FileCommands.CloseFile
                    } |> Async.StartImmediate
                    normalMode, resetKeys
                | ':', "wq!"  ->
                    async {
                        do! save()
                        do! forceClose()
                    } |> Async.StartImmediate
                    normalMode, resetKeys
                | ':', _  ->
                    { state with statusMessage = sprintf "Could not parse :%s" rest |> Some; mode = NormalMode; }
                    , resetKeys
                | _ -> normalMode, resetKeys
            | _ -> normalMode, resetKeys
        | _ ->
            let message =
                match state.statusMessage with
                | Some msg -> sprintf "%s%c" msg key.KeyChar
                | None -> string key.KeyChar

            setMessage (Some message), processCommand message

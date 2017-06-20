namespace XSVim
open MonoDevelop.Ide.Editor
open MonoDevelop.Ide.Editor.Extension

module exMode =
    let getFirstCharAndRest (s:string) = s.[0], s.[1..]

    let getNextSearchOffset (editor:TextEditor) (search:string) =
        let index = editor.Text.IndexOf(search, editor.CaretOffset)
        if index > -1 then
            Some index
        else
            let index = editor.Text.IndexOf(search)
            if index > -1 then
                Some index
            else
                None

    let processKey (editor:TextEditor) (state:VimState) (key:KeyDescriptor) =
        let command =
            match key.SpecialKey with
            | SpecialKey.BackSpace ->
                let message =
                    match state.statusMessage with
                    | Some msg ->
                        let len = msg.Length
                        msg.[0..len-2]
                    | None -> ""
                if message.Length > 0 then
                    Some message
                else
                    None
            | SpecialKey.Return ->
                state.statusMessage 
                |> Option.bind(fun message ->
                    let firstChar, rest = getFirstCharAndRest message
                    match firstChar, rest with
                    | '/', _ ->
                        getNextSearchOffset editor rest
                        |> Option.bind(fun index ->
                            editor.CaretOffset <- index
                            None)
                    | _ -> None)
            | SpecialKey.Escape ->
                None
            | _ ->
                match state.statusMessage with
                | Some msg -> sprintf "%s%c" msg key.KeyChar |> Some
                | None -> string key.KeyChar |> Some

        match command with
        | Some cmd ->
            let firstChar, rest = getFirstCharAndRest cmd
            match firstChar, rest with
            | '/', _ ->
                getNextSearchOffset editor rest
                |> Option.iter(fun index -> editor.SetSelection(index, index+rest.Length))
            | _ -> ()
            { state with statusMessage = command }
        | _ -> { state with statusMessage = None; mode = NormalMode }


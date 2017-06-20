namespace XSVim
open MonoDevelop.Ide.Editor
open MonoDevelop.Ide.Editor.Extension

module exMode =
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
            | SpecialKey.Escape ->
                None
            | _ ->
                match state.statusMessage with
                | Some msg -> sprintf "%s%c" msg key.KeyChar |> Some
                | None -> string key.KeyChar |> Some

        match command with
        | Some cmd ->
            let firstChar, rest =
                cmd.[0], cmd.[1..]
            match firstChar, rest with
            | '/', _ ->
                let index = editor.Text.IndexOf(rest, editor.CaretOffset)
                if index > -1 then
                    editor.SetSelection(index, index+rest.Length)
            | _ -> ()
            { state with statusMessage = command }
        | _ -> { state with statusMessage = None; mode = NormalMode }


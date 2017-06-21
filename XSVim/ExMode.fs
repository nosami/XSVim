namespace XSVim
open MonoDevelop.Ide.Editor
open MonoDevelop.Ide.Editor.Extension

module exMode =
    let getFirstCharAndRest (s:string) = s.[0], s.[1..]

    let processKey (editor:TextEditor) (state:VimState) (key:KeyDescriptor) =
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
                setMessage (Some message), wait
            else
                setMessage None, resetKeys
        | SpecialKey.Return ->
            match state.statusMessage with
            | Some message ->
                let firstChar, rest = getFirstCharAndRest message
                match firstChar, rest with
                | '/', _ ->
                    { state with statusMessage = None; mode = NormalMode; lastSearch = Some rest }
                    , [ runOnce Move (ToSearch rest)]
                | '?', _ ->
                    { state with statusMessage = None; mode = NormalMode; lastSearch = Some rest }
                    , [ runOnce Move (ToSearchBackwards rest)]
                | _ -> normalMode, resetKeys
            | _ -> normalMode, resetKeys
        | SpecialKey.Escape ->
            normalMode, resetKeys
        | _ ->
            let message =
                match state.statusMessage with
                | Some msg -> sprintf "%s%c" msg key.KeyChar
                | None -> string key.KeyChar

            let firstChar, rest = getFirstCharAndRest message
            match firstChar, rest with
            | '/', _ -> setMessage (Some message), [ runOnce (IncrementalSearch rest) Nothing ]
            | '?', _ -> setMessage (Some message), [ runOnce (IncrementalSearchBackwards rest) Nothing ]
            | _ -> setMessage (Some message), wait

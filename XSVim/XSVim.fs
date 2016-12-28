namespace XSVim

open System
open System.Globalization
open MonoDevelop.Ide.Editor
open MonoDevelop.Ide.Editor.Extension
open Mono.TextEditor

type CommandType =
    | Move
    | Select
    | Delete
    | Change
    | DoNothing

type TextObject =
    | Character
    | AWord
    | InnerWord
    | AWORD
    | InnerWORD
    | ASentence
    | InnerSentence
    | AParagraph
    | InnerParagraph
    | ABlock of char * char
    | InnerBlock of char * char
    | WholeLine
    // motions
    | Up
    | Down
    | Left
    | Right
    | FirstNonWhitespace
    | StartOfLine
    | EndOfLine
    | ToCharInclusive of c:char
    | ToCharExclusive of c:char
    | WordForwards of c:char
    | WORDForwards of c:char
    | WordBackwards of c:char
    | WORDBackwards of c:char
    | ForwardToEndOfWord
    | ForwardToEndOfWORD
    | BackwardToEndOfWord
    | BackwardToEndOfWORD
    | Nothing

type VimAction = {
    repeat: int
    commandType: CommandType
    textObject: TextObject
}

module VimHelpers =
    let findCharForwardsOnLine (editor:TextEditorData) (line:DocumentLine) character =
        seq { editor.Caret.Offset .. line.EndOffset }
        |> Seq.tryFind(fun index -> editor.Text.[index] = character)

    let findCharForwards (editor:TextEditorData) character =
        seq { editor.Caret.Offset .. editor.Text.Length }
        |> Seq.tryFind(fun index -> editor.Text.[index] = character)

    let findCharBackwards (editor:TextEditorData) character =
        seq { editor.Caret.Offset .. -1 .. 0 }
        |> Seq.tryFind(fun index -> editor.Text.[index] = character)

    let findCharRange (editor:TextEditorData) startChar endChar =
        findCharBackwards editor startChar, findCharForwards editor endChar

    let getRange (editor:TextEditorData) motion =
        let line = editor.GetLine editor.Caret.Line
        match motion with
        | Right -> 
            let line = editor.GetLine editor.Caret.Line
            editor.Caret.Offset, if editor.Caret.Column < line.Length then editor.Caret.Offset + 1 else editor.Caret.Offset
        | Left -> editor.Caret.Offset, if editor.Caret.Column > DocumentLocation.MinColumn then editor.Caret.Offset - 1 else editor.Caret.Offset
        | Up ->
            editor.Caret.Offset,
            if editor.Caret.Line > DocumentLocation.MinLine then
                let visualLine = editor.LogicalToVisualLine(editor.Caret.Line)
                let lineNumber = editor.VisualToLogicalLine(visualLine - 1)
                editor.LocationToOffset (new DocumentLocation(lineNumber, editor.Caret.Column))
            else
                editor.Caret.Offset
        | Down ->
            editor.Caret.Offset,
            if editor.Caret.Line < editor.Document.LineCount then
                let visualLine = editor.LogicalToVisualLine(editor.Caret.Line)
                let lineNumber = editor.VisualToLogicalLine(visualLine + 1)
                editor.LocationToOffset (new DocumentLocation(lineNumber, editor.Caret.Column))
            else
                editor.Caret.Offset
        | EndOfLine -> editor.Caret.Offset, line.EndOffset
        | StartOfLine -> editor.Caret.Offset, line.Offset
        | FirstNonWhitespace -> editor.Caret.Offset, line.Offset + editor.GetLineIndent(editor.Caret.Line).Length
        | WholeLine -> line.Offset, line.EndOffset
        | ToCharInclusive c ->
            match findCharForwardsOnLine editor line c with
            | Some index -> editor.Caret.Offset, index+1
            | None -> editor.Caret.Offset, editor.Caret.Offset
        | ToCharExclusive c ->
            match findCharForwardsOnLine editor line c with
            | Some index -> editor.Caret.Offset, index
            | None -> editor.Caret.Offset, editor.Caret.Offset
        | InnerBlock (startChar, endChar) ->
            match findCharRange editor startChar endChar with
            | Some start, Some finish -> start+1, finish
            | _, _ -> editor.Caret.Offset, editor.Caret.Offset
        | ABlock (startChar, endChar) ->
            match findCharRange editor startChar endChar with
            | Some start, Some finish -> start, finish+1
            | _, _ -> editor.Caret.Offset, editor.Caret.Offset
        | _ -> 0,0

type XSVim() =
    inherit TextEditorExtension()

    let (|Digit|_|) character =
        if Char.IsDigit character then
            Some (CharUnicodeInfo.GetDecimalDigitValue character)
        else
            None

    let (|OneToNine|_|) character =
        if character > '1' && character < '9' then
            Some (CharUnicodeInfo.GetDecimalDigitValue character)
        else
            None

    let (|BlockDelimiter|_|) character =
        let pairs =
            [ 
                '[', ('[', ']')
                ']', ('[', ']')
                '(', ('(', ')')
                ')', ('(', ')')
                '{', ('{', '}')
                '}', ('{', '}')
                '<', ('<', '>')
                '>', ('<', '>')
            ] |> dict
        if pairs.ContainsKey character then
            Some pairs.[character]
        else
            None

    let (|Movement|_|) character =
        match character with
        | 'h' -> Some Left
        | 'j' -> Some Down
        | 'k' -> Some Up
        | 'l' -> Some Right
        | '$' -> Some EndOfLine
        | '^' -> Some StartOfLine
        | '0' -> Some StartOfLine
        | '_' -> Some FirstNonWhitespace
        | _ -> None

    let (|Action|_|) character =
        match character with
        | 'd' -> Some Delete
        | 'c' -> Some Change
        | _ -> None

    let keys = ResizeArray<_>()
    let mutable textEditorData = null
    let getCommand (repeat: int option) commandType textObject =
        Some { repeat=(match repeat with | Some r -> r | None -> 1); commandType=commandType; textObject=textObject }

    let wait = getCommand (Some 1) DoNothing Nothing

    member x.RunCommand command =
        let start, finish = VimHelpers.getRange textEditorData command.textObject
        match command.commandType with
        | Move -> x.Editor.CaretOffset <- finish
        | Delete -> 
            x.Editor.SetSelection(start, finish)
            ClipboardActions.Cut textEditorData
        | _ -> ()

    override x.Initialize() =
        textEditorData <- x.Editor.GetContent<ITextEditorDataProvider>().GetTextEditorData()

    override x.KeyPress(descriptor:KeyDescriptor) =
        if descriptor.KeyChar = 'q' then
            // temp debug reset code
            keys.Clear()
            false
        else
        if descriptor.KeyChar <> '\000' then
            keys.Add descriptor.KeyChar
        let keyList = keys |> List.ofSeq

        let multiplier, keyList =
            match keyList with
            | OneToNine d1 :: Digit d2 :: Digit d3 :: Digit d4 :: t -> Some(d1 * 1000 + d2 * 100 + d3 * 10 + d4), t
            | OneToNine d1 :: Digit d2 :: Digit d3 :: t -> Some (d1 * 100 + d2 * 10 + d3), t
            | OneToNine d1 :: Digit d2 :: t -> Some (d1 * 10 + d2), t
            | OneToNine d :: t -> Some d,t
            | _ -> None, keyList

        let run = getCommand multiplier
        let action =
            match keyList with
            | [ Movement m ] -> run Move m
            | [ Action action; Movement m ] -> run action m
            | [ Action action; 'd' ] -> run action WholeLine
            | [ Action action; 't'; c ] -> run action (ToCharExclusive c)
            | [ Action action; 'f'; c ] -> run action (ToCharInclusive c)
            | [ Action action; 'i'; BlockDelimiter c ] -> run action (InnerBlock c)
            | [ Action action; 'a'; BlockDelimiter c ] -> run action (ABlock c)
            | [ Action action ] -> wait
            | [ Action action; _ ] -> wait
            | _ -> None

        match multiplier, action with
        | _, Some action' ->
            MonoDevelop.Core.LoggingService.LogDebug (sprintf "%A %A" keys action')
            if action'.commandType <> DoNothing then
                x.RunCommand action'
                keys.Clear()
            false
        | None, None -> base.KeyPress descriptor
        | _, _ -> false

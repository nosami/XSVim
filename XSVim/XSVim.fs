namespace XSVim
open System
open System.Collections.Generic
open MonoDevelop.Core
open MonoDevelop.Ide.Commands
open MonoDevelop.Ide.Editor
open MonoDevelop.Ide.Editor.Extension

type BeforeOrAfter = Before | After

type CaretMode = Insert | Block

type VimMode =
    | NormalMode
    | VisualMode
    | VisualBlockMode
    | VisualLineMode
    | InsertMode

type CommandType =
    | Move
    | Visual
    | Yank
    | Put of BeforeOrAfter
    | Delete
    | DeleteLeft
    | BlockInsert
    | Change
    | SwitchMode of VimMode
    | Undo
    | Redo
    | JoinLines
    | Dispatch of obj
    | InsertLine of BeforeOrAfter
    | ReplaceChar of string
    | ResetKeys
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
    | ABlock of string * string
    | InnerBlock of string * string
    | WholeLine
    | WholeLineIncludingDelimiter
    | LastLine
    // motions
    | Up
    | Down
    | Left
    | Right
    | RightIncludingDelimiter
    | EnsureCursorBeforeDelimiter
    | FirstNonWhitespace
    | StartOfLine
    | StartOfDocument
    | EndOfLine
    | EndOfLineIncludingDelimiter
    | ToCharInclusive of string
    | ToCharInclusiveBackwards of string
    | ToCharExclusive of string
    | ToCharExclusiveBackwards of string
    | WordForwards
    | WORDForwards
    | WordBackwards
    | WORDBackwards
    | ParagraphForwards
    | ParagraphBackwards
    | ForwardToEndOfWord
    | ForwardToEndOfWORD
    | BackwardToEndOfWord
    | BackwardToEndOfWORD
    | Nothing
    | HalfPageUp
    | HalfPageDown
    | PageUp
    | PageDown
    | CurrentLocation
    | Selection
    | SelectionStart
    | MatchingBrace

type VimAction = {
    repeat: int
    commandType: CommandType
    textObject: TextObject
}

type VimState = {
    keys: string list
    mode: VimMode
    visualStartOffset: int
    findCharCommand: VimAction option // f,F,t or T command to be repeated with ;
    lastAction: VimAction list // used by . command to repeat the last action
    clipboard: string
    desiredColumn: int
}

[<AutoOpen>]
module VimHelpers =
    let dispatch command = MonoDevelop.Ide.IdeApp.CommandService.DispatchCommand command |> ignore

    let closingBraces = [')'; '}'; ']'] |> set
    let openingbraces = ['('; '{'; '[' ] |> set

    let findNextBraceForwardsOnLine (editor:TextEditor) (line:IDocumentLine) =
        if closingBraces.Contains(editor.Text.[editor.CaretOffset]) then
            Some editor.CaretOffset
        else
            seq { editor.CaretOffset .. line.EndOffset }
            |> Seq.tryFind(fun index -> openingbraces.Contains(editor.Text.[index]))

    let findCharForwardsOnLine (editor:TextEditor) (line:IDocumentLine) character =
        let ch = Char.Parse character
        seq { editor.CaretOffset+1 .. line.EndOffset }
        |> Seq.tryFind(fun index -> editor.Text.[index] = ch)

    let findCharBackwardsOnLine (editor:TextEditor) (line:IDocumentLine) character =
        let ch = Char.Parse character
        seq { editor.CaretOffset-1 .. -1 .. line.Offset }
        |> Seq.tryFind(fun index -> editor.Text.[index] = ch)

    let findCharForwards (editor:TextEditor) character =
        let ch = Char.Parse character
        seq { editor.CaretOffset+1 .. editor.Text.Length }
        |> Seq.tryFind(fun index -> editor.Text.[index] = ch)

    let findCharBackwards (editor:TextEditor) character =
        let ch = Char.Parse character
        seq { editor.CaretOffset .. -1 .. 0 }
        |> Seq.tryFind(fun index -> editor.Text.[index] = ch)

    let findCharRange (editor:TextEditor) startChar endChar =
        findCharBackwards editor startChar, findCharForwards editor endChar

    let isWordChar c = Char.IsLetterOrDigit c || c = '-' || c = '_'

    let findWordForwards (editor:TextEditor) =
        let findFromNonLetterChar index =
            match editor.Text.[index] with
            | ' ' ->
                seq { index+1 .. editor.Text.Length } 
                |> Seq.tryFind(fun index -> not (Char.IsWhiteSpace editor.Text.[index]))
            | _ -> Some index

        if not (isWordChar editor.Text.[editor.CaretOffset]) && isWordChar editor.Text.[editor.CaretOffset + 1] then 
            editor.CaretOffset + 1 |> Some
        else
            seq { editor.CaretOffset+1 .. editor.Text.Length }
            |> Seq.tryFind(fun index -> not (isWordChar editor.Text.[index]))
            |> Option.bind findFromNonLetterChar

    let findPrevWord (editor:TextEditor) =
        let result = Math.Max(editor.CaretOffset - 1, 0)
        let previous = isWordChar editor.Text.[result]
        let rec findStartBackwards index previous isInIdentifier =
            let ch = editor.Text.[index]
            let current = isWordChar ch

            match previous with
            | _ when index = 0 -> 0
            | false when isInIdentifier -> index + 2
            | _ -> findStartBackwards (index - 1) current previous
        findStartBackwards result previous previous

    let findWordEnd (editor:TextEditor) =
        let result = Math.Min(editor.CaretOffset+1, editor.Text.Length)
        let previous = isWordChar editor.Text.[result]

        let rec findEnd index previous isInIdentifier =
            let ch = editor.Text.[index]
            let current = isWordChar ch

            match previous with
            | _ when index = editor.Text.Length-1 -> editor.Text.Length
            | false when isInIdentifier -> index - 2
            | _ -> findEnd (index + 1) current previous
        findEnd result previous previous

    let findCurrentWordStart (editor:TextEditor) =
        seq { editor.CaretOffset .. -1 .. 1 }
        |> Seq.tryFind(fun index -> not (isWordChar editor.Text.[index-1]))

    let findCurrentWordEnd (editor:TextEditor) =
        seq { editor.CaretOffset .. editor.Text.Length-1 }
        |> Seq.tryFind(fun index -> not (isWordChar editor.Text.[index]))

    let paragraphBackwards (editor:TextEditor) =
        seq { editor.CaretLine-1 .. -1 .. 1 }
        |> Seq.tryFind(fun lineNr -> let line = editor.GetLineText lineNr
                                     String.IsNullOrWhiteSpace line)
        |> Option.bind(fun lineNr -> Some (editor.GetLine lineNr).Offset)

    let paragraphForwards (editor:TextEditor) =
        seq { editor.CaretLine+1 .. editor.LineCount }
        |> Seq.tryFind(fun lineNr -> let line = editor.GetLineText lineNr
                                     String.IsNullOrWhiteSpace line)
        |> Option.bind(fun lineNr -> Some (editor.GetLine lineNr).Offset)

    let getVisibleLineCount (_editor:TextEditor) =
        //let topVisibleLine = ((editor .VAdjustment.Value / editor.LineHeight) |> int) + 1
        //let bottomVisibleLine =
        //    Math.Min(editor.LineCount - 1,
        //        topVisibleLine + ((editor.VAdjustment.PageSize / editor.LineHeight) |> int))
        //bottomVisibleLine - topVisibleLine
        40
    let getRange (vimState:VimState) (editor:TextEditor) motion =
        let line = editor.GetLine editor.CaretLine
        match motion with
        | Right ->
            let line = editor.GetLine editor.CaretLine
            editor.CaretOffset, if editor.CaretColumn < line.Length then editor.CaretOffset + 1 else editor.CaretOffset
        | RightIncludingDelimiter ->
            let line = editor.GetLine editor.CaretLine
            editor.CaretOffset, if editor.CaretColumn < line.LengthIncludingDelimiter then editor.CaretOffset + 1 else editor.CaretOffset
        | EnsureCursorBeforeDelimiter ->
            let line = editor.GetLine editor.CaretLine
            editor.CaretOffset, if editor.CaretColumn < line.Length then editor.CaretOffset else editor.CaretOffset - 1
        | Left -> editor.CaretOffset, if editor.CaretColumn > DocumentLocation.MinColumn then editor.CaretOffset - 1 else editor.CaretOffset
        | Up ->
            editor.CaretOffset,
            if editor.CaretLine > DocumentLocation.MinLine then
                let column =
                    if vimState.desiredColumn > editor.CaretColumn && vimState.desiredColumn <= editor.Length then
                        vimState.desiredColumn
                    else
                        editor.CaretColumn

                editor.LocationToOffset (new DocumentLocation(editor.CaretLine - 1, column))
            else
                editor.CaretOffset
        | Down ->
            editor.CaretOffset,
            if editor.CaretLine < editor.LineCount then
                let column =
                    if vimState.desiredColumn > editor.CaretColumn && vimState.desiredColumn <= editor.Length then
                        vimState.desiredColumn
                    else
                        editor.CaretColumn

                editor.LocationToOffset (new DocumentLocation(editor.CaretLine + 1, column))
            else
                editor.CaretOffset
        | EndOfLine -> editor.CaretOffset, line.EndOffset-1
        | EndOfLineIncludingDelimiter -> editor.CaretOffset, line.EndOffsetIncludingDelimiter
        | StartOfLine -> editor.CaretOffset, line.Offset
        | StartOfDocument -> editor.CaretOffset, 0
        | FirstNonWhitespace -> editor.CaretOffset, line.Offset + editor.GetLineIndent(editor.CaretLine).Length
        | WholeLine -> line.Offset, line.EndOffset
        | WholeLineIncludingDelimiter -> line.Offset, line.EndOffsetIncludingDelimiter
        | LastLine ->
            let lastLine = editor.GetLine editor.LineCount
            editor.CaretOffset, lastLine.Offset
        | ToCharInclusiveBackwards c ->
            match findCharBackwardsOnLine editor line c with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, editor.CaretOffset
        | ToCharExclusiveBackwards c ->
            match findCharBackwardsOnLine editor line c with
            | Some index -> editor.CaretOffset, index+1
            | None -> editor.CaretOffset, editor.CaretOffset
        | ToCharInclusive c ->
            match findCharForwardsOnLine editor line c with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, editor.CaretOffset
        | ToCharExclusive c ->
            match findCharForwardsOnLine editor line c with
            | Some index -> editor.CaretOffset, index-1
            | None -> editor.CaretOffset, editor.CaretOffset
        | InnerBlock (startChar, endChar) ->
            match findCharRange editor startChar endChar with
            | Some start, Some finish -> start+1, finish
            | _, _ -> editor.CaretOffset, editor.CaretOffset
        | ABlock (startChar, endChar) ->
            match findCharRange editor startChar endChar with
            | Some start, Some finish when finish < editor.Text.Length -> start, finish+1
            | _, _ -> editor.CaretOffset, editor.CaretOffset
        | WordForwards ->
            match findWordForwards editor with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, editor.CaretOffset
        | WordBackwards -> editor.CaretOffset, findPrevWord editor
        | ParagraphBackwards ->
            match paragraphBackwards editor with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, editor.CaretOffset
        | ParagraphForwards ->
            match paragraphForwards editor with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, editor.CaretOffset
        | InnerWord -> match findCurrentWordStart editor, findCurrentWordEnd editor with
                       | Some startpos, Some endpos -> startpos, endpos
                       | _ -> editor.CaretOffset, editor.CaretOffset
        | AWord -> match findCurrentWordStart editor, findCurrentWordEnd editor with
                   | Some startpos, Some endpos -> startpos, endpos
                   | _ -> editor.CaretOffset, editor.CaretOffset
        | ForwardToEndOfWord -> editor.CaretOffset, findWordEnd editor
        //| BackwardToEndOfWord -> editor.CaretOffset, findPrevWord editor |> editor.FindCurrentWordEnd
        | HalfPageUp -> 
            let visibleLineCount = getVisibleLineCount editor
            let halfwayUp = Math.Max(1, editor.CaretLine - visibleLineCount / 2)
            editor.CaretOffset, editor.GetLine(halfwayUp).Offset
        | HalfPageDown -> 
            let visibleLineCount = getVisibleLineCount editor
            let halfwayDown = Math.Min(editor.LineCount, editor.CaretLine + visibleLineCount / 2)
            editor.CaretOffset, editor.GetLine(halfwayDown).Offset
        | PageUp ->
            let visibleLineCount = getVisibleLineCount editor
            let pageUp = Math.Max(1, editor.CaretLine - visibleLineCount)
            editor.CaretOffset, editor.GetLine(pageUp).Offset
        | PageDown ->
            let visibleLineCount = getVisibleLineCount editor
            let pageDown = Math.Min(editor.LineCount, editor.CaretLine + visibleLineCount)
            editor.CaretOffset, editor.GetLine(pageDown).Offset
        | CurrentLocation -> editor.CaretOffset, editor.CaretOffset+1
        | Selection ->
            let selection = editor.Selections |> Seq.head
            let lead = editor.LocationToOffset selection.Lead
            let anchor = editor.LocationToOffset selection.Anchor
            Math.Min(lead, anchor), Math.Max(lead, anchor)
        | SelectionStart -> editor.CaretOffset, vimState.visualStartOffset
        | MatchingBrace ->
            match findNextBraceForwardsOnLine editor line with
            | Some offset ->
                let startOffset = editor.CaretOffset
                editor.CaretOffset <- offset
                dispatch TextEditorCommands.GotoMatchingBrace
                startOffset, editor.CaretOffset
            | _ -> editor.CaretOffset, editor.CaretOffset
        | _ -> editor.CaretOffset, editor.CaretOffset

module Vim =
    let (|VisualModes|_|) mode =
        match mode with
        | VisualMode | VisualLineMode | VisualBlockMode -> Some VisualModes
        | _ -> None

    let (|NotInsertMode|_|) mode =
        if mode = InsertMode then None else Some NotInsertMode

    let setSelection vimState (editor:TextEditor) (command:VimAction) (start:int) finish =
        match vimState.mode, command.commandType with
        | VisualMode, Move | VisualMode, SwitchMode _ ->
            let start, finish =
                if finish < vimState.visualStartOffset then
                    finish, vimState.visualStartOffset + 1
                else
                    vimState.visualStartOffset, finish + if command.textObject = EndOfLine then 0 else 1
            editor.SetSelection(start, finish)
        | VisualBlockMode, Move | VisualBlockMode, SwitchMode _ ->
            let selectionStartLocation = editor.OffsetToLocation vimState.visualStartOffset
            let leftColumn, rightColumn =
                if editor.CaretColumn < selectionStartLocation.Column then
                    editor.CaretColumn, selectionStartLocation.Column+1
                else
                    selectionStartLocation.Column, editor.CaretColumn+1
            let topLine = Math.Min(selectionStartLocation.Line, editor.CaretLine)
            let bottomLine = Math.Max(selectionStartLocation.Line, editor.CaretLine)
            editor.SetSelection(new DocumentLocation (topLine, leftColumn), new DocumentLocation (bottomLine, rightColumn))
            if editor.SelectionMode = SelectionMode.Normal then dispatch TextEditorCommands.ToggleBlockSelectionMode
        | VisualLineMode, Move | VisualLineMode, SwitchMode _ ->
            let startPos = Math.Min(finish, vimState.visualStartOffset)
            let endPos = Math.Max(finish, vimState.visualStartOffset)
            let startLine = editor.GetLineByOffset startPos
            let endLine = editor.GetLineByOffset endPos
            editor.SetSelection(startLine.Offset, endLine.EndOffsetIncludingDelimiter)
        | _ -> editor.SetSelection(start, finish)

    let (|MoveUpOrDown|_|) command =
        match command with
        | { commandType=Move; textObject=Up}
        | { commandType=Move; textObject=Down} -> Some MoveUpOrDown
        | _ -> None

    let runCommand vimState editor command =
        let delete state start finish =
            let finish =
                match command.textObject with
                | ForwardToEndOfWord
                | EndOfLine
                | ToCharInclusive _
                | ToCharExclusive _ -> finish + 1
                | _ -> finish
            if command.textObject <> Selection then
                setSelection state editor command start finish
            let clipboard = editor.SelectedText
            EditActions.ClipboardCut editor
            { state with clipboard = clipboard }

        let setCaretMode caretMode =
            match vimState.mode, caretMode with
            | NotInsertMode, Insert -> EditActions.SwitchCaretMode editor
            | InsertMode, Block -> EditActions.SwitchCaretMode editor
            | _ -> ()

        let switchToInsertMode state =
            setCaretMode Insert
            { state with mode = InsertMode; keys = [] }

        let rec processCommands count vimState = 
            let start, finish = VimHelpers.getRange vimState editor command.textObject
            let newState =
                match command.commandType with
                | Move ->
                    match vimState.mode with
                    | VisualModes ->
                        editor.CaretOffset <- finish
                        let finish =
                            match command.textObject with
                            | EndOfLine -> finish + 1
                            | _ -> finish

                        setSelection vimState editor command start finish
                    | _ -> ()
                    let newState =
                        match command, vimState.lastAction with
                        // don't change desired column if we already started moving up or down
                        | MoveUpOrDown, [ MoveUpOrDown ] -> vimState 
                        | _ -> { vimState with desiredColumn = editor.CaretColumn }
                    editor.CaretOffset <- finish
                    newState

                | Delete -> delete vimState start finish
                | DeleteLeft -> if editor.CaretColumn > 1 then delete vimState (editor.CaretOffset - 1) editor.CaretOffset else vimState
                | Change -> 
                    let state = delete vimState start finish
                    switchToInsertMode state
                | Yank ->
                    let finish =
                        match command.textObject with
                        | ForwardToEndOfWord
                        | EndOfLine
                        | ToCharInclusive _
                        | ToCharExclusive _ -> finish + 1
                        | _ -> finish
                    if command.textObject <> Selection then
                        setSelection vimState editor command start finish
                    let clipboard = editor.SelectedText
                    EditActions.ClipboardCopy editor
                    LoggingService.LogDebug (sprintf "Yanked - %s" clipboard)
                    editor.ClearSelection()
                    { vimState with clipboard = clipboard }
                | Put Before -> 
                    if vimState.clipboard.EndsWith "\n" then
                        editor.CaretOffset <- editor.GetLine(editor.CaretLine).Offset
                        EditActions.ClipboardPaste editor
                        EditActions.MoveCaretUp editor
                    else
                        EditActions.ClipboardPaste editor
                    vimState
                | Put After ->
                    if vimState.clipboard.EndsWith "\n" then
                        editor.CaretOffset <- editor.GetLine(editor.CaretLine).EndOffset+1
                        EditActions.ClipboardPaste editor
                        EditActions.MoveCaretUp editor
                    else
                        EditActions.MoveCaretRight editor
                        EditActions.ClipboardPaste editor
                        EditActions.MoveCaretLeft editor
                    vimState
                | Visual ->
                    editor.SetSelection(start, finish); vimState
                | Undo -> EditActions.Undo editor; vimState
                | Redo -> EditActions.Redo editor; vimState
                | JoinLines -> EditActions.JoinLines editor; vimState
                | ReplaceChar c ->
                    editor.SetSelection(editor.CaretOffset, editor.CaretOffset+1)
                    EditActions.Delete editor
                    editor.InsertAtCaret c
                    EditActions.MoveCaretLeft editor
                    vimState
                | InsertLine Before -> 
                    EditActions.InsertNewLineAtEnd editor
                    vimState

                | InsertLine After -> 
                    EditActions.MoveCaretUp editor
                    EditActions.InsertNewLineAtEnd editor
                    vimState
                | Dispatch command -> dispatch command ; vimState
                | ResetKeys -> { vimState with keys = [] }
                | BlockInsert ->
                    let selectionStartLocation = editor.OffsetToLocation vimState.visualStartOffset
                    let topLine = Math.Min(selectionStartLocation.Line, editor.CaretLine)
                    let bottomLine = Math.Max(selectionStartLocation.Line, editor.CaretLine)
                    editor.CaretColumn <- Math.Min(editor.CaretColumn, selectionStartLocation.Column)
                    editor.SetSelection(new DocumentLocation (topLine, selectionStartLocation.Column),new DocumentLocation (bottomLine, selectionStartLocation.Column))
                    if editor.SelectionMode = SelectionMode.Normal then dispatch TextEditorCommands.ToggleBlockSelectionMode
                    switchToInsertMode vimState
                | SwitchMode mode ->
                    match mode with
                    | NormalMode -> 
                        editor.ClearSelection()
                        setCaretMode Block
                        { vimState with mode = mode }
                    | VisualMode | VisualLineMode | VisualBlockMode ->
                        setCaretMode Block
                        let start, finish = VimHelpers.getRange vimState editor command.textObject
                        let newState = { vimState with mode = mode; visualStartOffset = editor.CaretOffset }
                        setSelection newState editor command start finish
                        match mode, editor.SelectionMode with
                        | VisualBlockMode, SelectionMode.Normal -> dispatch TextEditorCommands.ToggleBlockSelectionMode
                        | _, SelectionMode.Block -> dispatch TextEditorCommands.ToggleBlockSelectionMode
                        | _ -> ()
                        newState
                    | InsertMode -> switchToInsertMode vimState
                | _ -> vimState
            if count = 1 then newState else processCommands (count-1) newState
        processCommands command.repeat vimState

    let (|Digit|_|) character =
        if character >= "0" && character <= "9" then
            Some (Convert.ToInt32 character)
        else
            None

    let (|OneToNine|_|) character =
        if character >= "1" && character <= "9" then
            Some (Convert.ToInt32 character)
        else
            None

    let (|BlockDelimiter|_|) character =
        let pairs =
            [ 
                "[", ("[", "]")
                "]", ("[", "]")
                "(", ("(", ")")
                ")", ("(", ")")
                "{", ("{", "}")
                "}", ("{", "}")
                "<", ("<", ">")
                ">", ("<", ">")
                "\"", ("\"", "\"")
                "'", ("'", "'")
            ] |> dict
        if pairs.ContainsKey character then
            Some pairs.[character]
        else
            None

    let (|Movement|_|) character =
        match character with
        | "h" -> Some Left
        | "j" -> Some Down
        | "k" -> Some Up
        | "l" -> Some Right
        | "$" -> Some EndOfLine
        | "^" -> Some FirstNonWhitespace
        | "0" -> Some StartOfLine
        | "_" -> Some FirstNonWhitespace
        | "w" -> Some WordForwards
        | "b" -> Some WordBackwards
        | "e" -> Some ForwardToEndOfWord
        | "E" -> Some BackwardToEndOfWord
        | "G" -> Some LastLine
        | "{" -> Some ParagraphBackwards
        | "}" -> Some ParagraphForwards
        | "%" -> Some MatchingBrace
        | "<C-d>" -> Some HalfPageDown
        | "<C-u>" -> Some HalfPageUp
        | "<C-f>" -> Some PageDown
        | "<C-b>" -> Some PageUp
        | _ -> None

    let (|FindChar|_|) character =
        match character with
        | "f" -> Some ToCharInclusive
        | "F" -> Some ToCharInclusiveBackwards
        | "t" -> Some ToCharExclusive
        | "T" -> Some ToCharExclusiveBackwards
        | _ -> None

    let (|Action|_|) character =
        match character with
        | "d" -> Some Delete
        | "c" -> Some Change
        | "v" -> Some Visual
        | "y" -> Some Yank
        | _ -> None

    let (|ModeChange|_|) character =
        match character with
        | "i" -> Some InsertMode
        | "v" -> Some VisualMode
        | "<C-v>" -> Some VisualBlockMode
        | "V" -> Some VisualLineMode
        | _ -> None

    let (|Escape|_|) character =
        match character with
        | "<esc>" | "<C-c>" | "<C-[>" -> Some Escape
        | _ -> None

    let getCommand repeat commandType textObject =
        { repeat=repeat; commandType=commandType; textObject=textObject }

    let wait = [ getCommand 1 DoNothing Nothing ]

    let parseKeys (state:VimState) =
        let keyList = state.keys
        let multiplier, keyList =
            match keyList with
            | "r" :: _ -> 1, keyList
            // d2w -> 2, dw
            | c :: OneToNine d1 :: Digit d2 :: Digit d3 :: Digit d4 :: t ->
                d1 * 1000 + d2 * 100 + d3 * 10 + d4, c::t
            | c :: OneToNine d1 :: Digit d2 :: Digit d3 :: t ->
                d1 * 100 + d2 * 10 + d3, c::t
            | c :: OneToNine d1 :: Digit d2 :: t ->
                d1 * 10 + d2, c::t
            | c :: OneToNine d :: t ->
                d, c::t
            // 2dw -> 2, dw
            | OneToNine d1 :: Digit d2 :: Digit d3 :: Digit d4 :: t ->
                d1 * 1000 + d2 * 100 + d3 * 10 + d4, t
            | OneToNine d1 :: Digit d2 :: Digit d3 :: t ->
                d1 * 100 + d2 * 10 + d3, t
            | OneToNine d1 :: Digit d2 :: t ->
                d1 * 10 + d2, t
            | OneToNine d :: t -> d, t
            | _ -> 1, keyList

        let run = getCommand multiplier
        let switchMode mode = run (SwitchMode mode) Nothing
        let dispatch command = run (Dispatch command) Nothing

        LoggingService.LogDebug (sprintf "%A %A" state.mode keyList)
        let newState =
            match keyList with
            | [ FindChar m; c ] -> { state with findCharCommand = run Move ( m c ) |> Some }
            | _ -> state

        let action =
            match state.mode, keyList with
            | VisualBlockMode, [ Escape ] -> [ run Move SelectionStart; switchMode NormalMode ]
            | NormalMode, [ Escape ] -> [ run ResetKeys Nothing ]
            | _, [ Escape ] -> [ run (SwitchMode NormalMode) Nothing; run Move Left ]
            | NotInsertMode, [ Movement m ] -> [ run Move m ]
            | NotInsertMode, [ FindChar m; c ] -> [ run Move (m c) ]
            | NormalMode, [ Action action; Movement m ] -> [ run action m ]
            | NormalMode, [ "u" ] -> [ run Undo Nothing ]
            | NormalMode, [ "<C-r>" ] -> [ run Redo Nothing ]
            | NormalMode, [ "d"; "d" ] -> [ run Delete WholeLineIncludingDelimiter ]
            | NormalMode, [ "c"; "c" ] -> [ run Change WholeLine ]
            | NormalMode, [ "y"; "y" ] -> [ run Yank WholeLineIncludingDelimiter ]
            | NormalMode, [ "Y" ] -> [ run Yank WholeLineIncludingDelimiter ]
            | NormalMode, [ "C" ] -> [ run Change EndOfLine ]
            | NormalMode, [ "D" ] -> [ run Delete EndOfLine ]
            | NormalMode, [ "x" ] -> [ run Delete CurrentLocation; run Move EnsureCursorBeforeDelimiter ]
            | NormalMode, [ "X" ] -> [ run DeleteLeft Nothing ]
            | NormalMode, [ "s"] -> [ run Delete CurrentLocation; run Move EnsureCursorBeforeDelimiter; switchMode InsertMode ]
            | NormalMode, [ "p" ] -> [ run (Put After) Nothing ]
            | NormalMode, [ "P" ] -> [ run (Put Before) Nothing ]
            | NormalMode, [ "J" ] -> [ run JoinLines Nothing ]
            | NormalMode, [ "/" ] -> [ dispatch SearchCommands.Find ]
            | NormalMode, [ "n" ] -> [ dispatch SearchCommands.FindNext ]
            | NormalMode, [ "N" ] -> [ dispatch SearchCommands.FindPrevious ]
            | NormalMode, [ "z"; "z" ] -> [ dispatch TextEditorCommands.RecenterEditor ]
            | NormalMode, [ "z"; ] -> wait
            | NormalMode, [ "<C-y>" ] -> [ dispatch TextEditorCommands.ScrollLineUp ]
            | NormalMode, [ "<C-e>" ] -> [ dispatch TextEditorCommands.ScrollLineDown ]
            | NormalMode, [ "r" ] -> wait
            | NormalMode, [ "r"; c ] -> [ run (ReplaceChar c) Nothing ]
            | NotInsertMode, [ Action action; FindChar m; c ] -> [ run action (m c) ]
            | NotInsertMode, [ Action action; "i"; BlockDelimiter c ] -> [ run action (InnerBlock c) ]
            | NotInsertMode, [ Action action; "a"; BlockDelimiter c ] -> [ run action (ABlock c) ]
            | NotInsertMode, [ Action action; "i"; "w" ] -> [ run action InnerWord ]
            | NotInsertMode, [ Action action; "a"; "w" ] -> [ run action AWord ]
            | VisualMode, [ "i"; "w" ] -> [ run Visual InnerWord ]
            | VisualMode, [ "a"; "w" ] -> [ run Visual AWord ]
            | VisualMode, [ "u"] -> [ dispatch EditCommands.LowercaseSelection ]
            | VisualMode, [ "U"] -> [ dispatch EditCommands.UppercaseSelection ]
            | NormalMode, [ ModeChange mode ] -> [ switchMode mode ]
            | NormalMode, [ "a" ] -> [ run Move RightIncludingDelimiter; switchMode InsertMode ]
            | NormalMode, [ "A" ] -> [ run Move EndOfLine; switchMode InsertMode ]
            | NormalMode, [ "O" ] -> [ run (InsertLine After) Nothing; switchMode InsertMode ]
            | NormalMode, [ "o" ] -> [ run (InsertLine Before) Nothing; switchMode InsertMode ]
            | NormalMode, [ "I" ] -> [ run Move FirstNonWhitespace; switchMode InsertMode ]
            | NormalMode, [ Action _ ] -> wait
            | NotInsertMode, [ Action _; "i" ] -> wait
            | NotInsertMode, [ Action _; "a" ] -> wait
            | VisualMode, [ "i" ] | VisualMode, [ "a" ] -> wait
            | NotInsertMode, [ FindChar _; ] -> wait
            | NotInsertMode, [ Action _; FindChar _; ] -> wait
            | NormalMode, [ "g" ] -> wait
            | NormalMode, [ "g"; "g" ] -> [ run Move StartOfDocument ]
            | NormalMode, [ "g"; "d" ] -> [ dispatch "MonoDevelop.Refactoring.RefactoryCommands.GotoDeclaration" ]
            | NormalMode, [ "g"; "t" ] -> [ dispatch WindowCommands.NextDocument ]
            | NormalMode, [ "g"; "T" ] -> [ dispatch WindowCommands.PrevDocument ]
            | NormalMode, [ "." ] -> state.lastAction
            | NormalMode, [ ";" ] -> match state.findCharCommand with Some command -> [ command ] | None -> []
            | VisualModes, [ Movement m ] -> [ run Move m ]
            | VisualBlockMode, [ "I" ] -> [ run BlockInsert Nothing; ]
            | VisualModes, [ "i"; BlockDelimiter c ] -> [ run Visual (InnerBlock c) ]
            | VisualModes, [ "a"; BlockDelimiter c ] -> [ run Visual (ABlock c) ]
            | VisualModes, [ "x" ] -> [ run Delete Selection; switchMode NormalMode ]
            | VisualModes, [ "d" ] -> [ run Delete Selection; switchMode NormalMode ]
            | VisualModes, [ "c" ] -> [ run Change Selection ]
            | VisualModes, [ "y" ] -> [ run Yank Selection; switchMode NormalMode ]
            | VisualModes, [ "Y" ] -> [ run Yank WholeLineIncludingDelimiter; switchMode NormalMode ]
            | NotInsertMode, [ ">" ] -> [ dispatch EditCommands.IndentSelection ]
            | NotInsertMode, [ "<" ] -> [ dispatch EditCommands.UnIndentSelection ]
            | NotInsertMode, [ "<C-w>" ] -> wait
            | NotInsertMode, [ "<C-w>"; "w" ]
            | NotInsertMode, [ "<C-w>"; "<C-w>" ] -> [ dispatch WindowCommands.NextDocument ]
            // These commands don't work the same way as vim yet, but better than nothing
            | NotInsertMode, [ "<C-w>"; "o" ] -> [ dispatch FileTabCommands.CloseAllButThis ]
            | NotInsertMode, [ "<C-w>"; "c" ] -> [ dispatch FileCommands.CloseFile ]
            | NotInsertMode, [ "<C-w>"; "v" ]
            | NotInsertMode, [ "<C-w>"; "s" ] 
            | NotInsertMode, [ "<C-w>"; "<C-v>" ]
            | NotInsertMode, [ "<C-w>"; "<C-s>" ] 
                -> [ dispatch "MonoDevelop.Ide.Commands.ViewCommands.SideBySideMode" ]
            | InsertMode, [ "<C-n>" ] -> [ dispatch TextEditorCommands.DynamicAbbrev ]
            | _, [] when multiplier > 1 -> wait
            | _ -> [ run ResetKeys Nothing ]
        multiplier, action, newState

    let handleKeyPress state (keyPress:KeyDescriptor) editorData =
        let newKeys =
            match state.mode, keyPress.KeyChar with
            | _, c when keyPress.ModifierKeys = ModifierKeys.Control ->
                state.keys @ [sprintf "<C-%c>" c]
            | NotInsertMode, c when keyPress.KeyChar <> '\000' ->
                state.keys @ [c |> string]
            | _ ->
                match keyPress.SpecialKey with
                | SpecialKey.Escape -> ["<esc>"]
                | SpecialKey.Left -> ["h"]
                | SpecialKey.Down -> ["j"]
                | SpecialKey.Up -> ["k"]
                | SpecialKey.Right -> ["l"]
                | _ -> state.keys
        let newState = { state with keys = newKeys }
        let multiplier, action, newState = parseKeys newState
        LoggingService.LogDebug (sprintf "%A %A" multiplier action)
        let rec performActions actions' state handled =
            match actions' with
            | [] -> state, handled
            | h::t ->
                if h.commandType = DoNothing then
                    newState, true
                else
                    let newState = runCommand state editorData h
                    performActions t { newState with keys = [] } true

        let newState, handled = performActions action newState false
        { newState with lastAction = action }, handled

type XSVim() =
    inherit TextEditorExtension()
    static let editorStates = Dictionary<FilePath, VimState>()

    override x.Initialize() =
        if not (editorStates.ContainsKey x.Editor.FileName) then
            editorStates.Add(x.Editor.FileName, { keys=[]; mode=NormalMode; visualStartOffset=0; findCharCommand=None; lastAction=[]; clipboard=""; desiredColumn=0 })
        EditActions.SwitchCaretMode x.Editor

    override x.KeyPress descriptor =
        let vimState = editorStates.[x.Editor.FileName]
        let oldState = vimState

        let newState, handledKeyPress = Vim.handleKeyPress vimState descriptor x.Editor
        editorStates.[x.Editor.FileName] <- newState
        match oldState.mode with
        | InsertMode -> base.KeyPress descriptor
        | VisualMode -> false
        | _ -> not handledKeyPress

namespace XSVim
open System
open System.Collections.Generic
open System.Text.RegularExpressions
open MonoDevelop.Core
open MonoDevelop.Core.Text
open MonoDevelop.Ide
open MonoDevelop.Ide.Commands
open MonoDevelop.Ide.Editor
open MonoDevelop.Ide.Editor.Extension

[<AutoOpen>]
module VimHelpers =
    let dispatch command = IdeApp.CommandService.DispatchCommand command |> ignore

    let closingBraces = [')'; '}'; ']'] |> set
    let openingbraces = ['('; '{'; '[' ] |> set

    let findNextBraceForwardsOnLine (editor:TextEditor) (line:IDocumentLine) =
        if closingBraces.Contains(editor.[editor.CaretOffset]) then
            Some editor.CaretOffset
        else
            seq { editor.CaretOffset .. line.EndOffset }
            |> Seq.tryFind(fun index -> openingbraces.Contains(editor.[index]))

    let findCharForwardsOnLine (editor:TextEditor) (line:IDocumentLine) startOffset character =
        let ch = Char.Parse character
        seq { startOffset+1 .. line.EndOffset }
        |> Seq.tryFind(fun index -> editor.[index] = ch)

    let findCharBackwardsOnLine startOffset (editor:TextEditor) (line:IDocumentLine) matcher =
        seq { startOffset .. -1 .. line.Offset }
        |> Seq.tryFind (fun i -> matcher editor.[i])

    let findCharBackwardsOnLineExclusive (editor:TextEditor) startOffset = findCharBackwardsOnLine (startOffset-1) editor
    let findCharBackwardsOnLineInclusive (editor:TextEditor) = findCharBackwardsOnLine editor.CaretOffset editor

    let findStringCharBackwardsOnLine (editor:TextEditor) (line:IDocumentLine) startOffset character =
        let ch = Char.Parse character
        let f = findCharBackwardsOnLineExclusive editor startOffset
        f line ((=) ch)

    let findCharForwards (editor:TextEditor) character =
        let ch = Char.Parse character
        seq { editor.CaretOffset+1 .. editor.Text.Length-1 }
        |> Seq.tryFind(fun index -> editor.[index] = ch)

    let findCharBackwards (editor:TextEditor) character =
        let ch = Char.Parse character
        seq { editor.CaretOffset .. -1 .. 0 }
        |> Seq.tryFind(fun index -> editor.[index] = ch)

    let findCharRange (editor:TextEditor) startChar endChar =
        findCharBackwards editor startChar, findCharForwards editor endChar

    let isWordChar c = Char.IsLetterOrDigit c || c = '-' || c = '_'
    let isWORDChar c = not (Char.IsWhiteSpace c)

    let (|InvisibleChar|_|) c =
        if Char.IsWhiteSpace c || c = '\r' || c = '\n' then
            Some InvisibleChar
        else
            None

    let findWordForwards (editor:TextEditor) commandType fWordChar =
        let findFromNonLetterChar index =
            match editor.[index], commandType with
            | InvisibleChar, Move ->
                seq { index+1 .. editor.Text.Length-1 } 
                |> Seq.tryFind(fun index -> not (Char.IsWhiteSpace editor.[index]))
            | _ -> Some index

        if not (fWordChar editor.[editor.CaretOffset]) && fWordChar editor.[editor.CaretOffset + 1] then 
            editor.CaretOffset + 1 |> Some
        else
            seq { editor.CaretOffset+1 .. editor.Text.Length-1 }
            |> Seq.tryFind(fun index -> index = editor.Text.Length-1 || not (fWordChar editor.[index]))
            |> Option.bind findFromNonLetterChar

    let findWordBackwards (editor:TextEditor) commandType fWordChar =
        let findFromNonLetterChar index =
            match editor.[index], commandType with
            | InvisibleChar, Move ->
                seq { index .. -1 .. 0 } 
                |> Seq.tryFind(fun index -> not (Char.IsWhiteSpace editor.[index]))
            | _ -> Some index

        if not (fWordChar editor.[editor.CaretOffset]) && fWordChar editor.[editor.CaretOffset + 1] then 
            editor.CaretOffset - 1 |> Some
        else
            seq { editor.CaretOffset .. -1 .. 0 }
            |> Seq.tryFind(fun index -> index = editor.Text.Length+1 || not (fWordChar editor.[index]))
            |> Option.bind findFromNonLetterChar

    let findPrevWord (editor:TextEditor) fWordChar =
        let result = Math.Max(editor.CaretOffset - 1, 0)
        let previous = fWordChar editor.[result]
        let rec findStartBackwards index previous isInIdentifier =
            let ch = editor.[index]
            let current = fWordChar ch

            match previous with
            | _ when index = 0 -> 0
            | false when isInIdentifier -> index + 2
            | _ -> findStartBackwards (index - 1) current previous
        findStartBackwards result previous previous

    let findWordEnd (editor:TextEditor) fWordChar =
        let result = Math.Min(editor.CaretOffset+1, editor.Text.Length-1)
        let previous = isWordChar editor.[result]

        let rec findEnd index previous isInIdentifier =
            let ch = editor.[index]
            let current = fWordChar ch

            match previous with
            | _ when index = editor.Text.Length-1 -> editor.Text.Length
            | false when isInIdentifier -> index - 2
            | _ -> findEnd (index + 1) current previous
        findEnd result previous previous

    let findCurrentWordStart (editor:TextEditor) fWordChar =
        seq { editor.CaretOffset .. -1 .. 1 }
        |> Seq.tryFind(fun index -> not (fWordChar editor.[index-1]))
        |> Option.defaultValue 0

    let findCurrentWordEnd (editor:TextEditor) fWordChar =
        seq { editor.CaretOffset .. editor.Text.Length-1 }
        |> Seq.tryFind(fun index -> not (fWordChar editor.[index]))
        |> Option.defaultValue (editor.Text.Length - 1)

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

    let wordAtCaret (editor:TextEditor) =
        if isWordChar (editor.[editor.CaretOffset]) then
            let start = findCurrentWordStart editor isWordChar
            let finish = (findWordEnd editor isWordChar)
            let finish = Math.Min (finish+1, editor.Text.Length)
            let word = editor.GetTextAt(start, finish - start)
            Some word
        else
            None

    let eofOnLine (line: IDocumentLine) =
        line.EndOffset = line.EndOffsetIncludingDelimiter

    let getRange (vimState:VimState) (editor:TextEditor) (command:VimAction) =
        let line = editor.GetLine editor.CaretLine
        match command.textObject with
        | Right ->
            let line = editor.GetLine editor.CaretLine
            editor.CaretOffset, if editor.CaretColumn < line.Length then editor.CaretOffset + 1 else editor.CaretOffset
        | RightIncludingDelimiter ->
            let line = editor.GetLine editor.CaretLine
            editor.CaretOffset,
            if line.Length > 0 && editor.CaretColumn <= line.LengthIncludingDelimiter then
                editor.CaretOffset + 1
            else
                editor.CaretOffset
        | Left ->
           editor.CaretOffset,
           if editor.CaretColumn > DocumentLocation.MinColumn && editor.[editor.CaretOffset-1] <> '\n' then
               editor.CaretOffset - 1
           else
               editor.CaretOffset
        | Up ->
            editor.CaretOffset,
            if editor.CaretLine > DocumentLocation.MinLine then
                let column =
                    let line = editor.GetLine (editor.CaretLine - 1)
                    let desiredColumn = vimState.desiredColumn |> Option.defaultValue editor.CaretColumn
                    if desiredColumn <= line.Length then
                        desiredColumn
                    else
                        line.Length

                editor.LocationToOffset (new DocumentLocation(editor.CaretLine - 1, column))
            else
                editor.CaretOffset
        | Down ->
            editor.CaretOffset,
            if editor.CaretLine < editor.LineCount then
                let column =
                    let line = editor.GetLine (editor.CaretLine + 1)
                    let desiredColumn = vimState.desiredColumn |> Option.defaultValue editor.CaretColumn
                    if desiredColumn <= line.Length then
                        desiredColumn
                    else
                        line.Length

                editor.LocationToOffset (new DocumentLocation(editor.CaretLine + 1, column))
            else
                editor.CaretOffset
        | EndOfLine -> editor.CaretOffset, line.EndOffset-1
        | EndOfLineIncludingDelimiter ->
            editor.CaretOffset,
            if eofOnLine line then
                line.EndOffsetIncludingDelimiter
            else
                line.EndOffsetIncludingDelimiter-1
        | StartOfLine -> editor.CaretOffset, line.Offset
        | StartOfLineNumber lineNumber ->
            let line = editor.GetLine lineNumber
            editor.CaretOffset, line.Offset
        | StartOfDocument -> editor.CaretOffset, 0
        | FirstNonWhitespace -> editor.CaretOffset, line.Offset + editor.GetLineIndent(editor.CaretLine).Length
        | WholeLine ->
            line.Offset, line.EndOffset
        | WholeLineIncludingDelimiter ->
            if eofOnLine line && line.LineNumber <> 1 then
                line.Offset-1, line.EndOffsetIncludingDelimiter
            else
                line.Offset, line.EndOffsetIncludingDelimiter
        | LastLine ->
            let lastLine = editor.GetLine editor.LineCount
            editor.CaretOffset, lastLine.Offset
        | ToCharInclusiveBackwards c ->
            match findStringCharBackwardsOnLine editor line (editor.CaretOffset-1) c with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, editor.CaretOffset
        | ToCharExclusiveBackwards c ->
            let startOffset =
                match vimState.keys with
                | ";" :: _ when c = editor.[editor.CaretOffset-1].ToString() ->
                    editor.CaretOffset-1
                | _ -> editor.CaretOffset
            match findStringCharBackwardsOnLine editor line startOffset c with
            | Some index -> editor.CaretOffset, index+1
            | None -> editor.CaretOffset, editor.CaretOffset
        | ToCharInclusive c ->
            match findCharForwardsOnLine editor line editor.CaretOffset c with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, editor.CaretOffset
        | ToCharExclusive c ->
            let startOffset =
                match vimState.keys with
                | ";" :: _ when c = editor.[editor.CaretOffset+1].ToString() ->
                    editor.CaretOffset+1
                | _ -> editor.CaretOffset
            match findCharForwardsOnLine editor line startOffset c with
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
            match findWordForwards editor command.commandType isWordChar with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, editor.CaretOffset
        | WORDForwards ->
            match findWordForwards editor command.commandType isWORDChar with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, editor.CaretOffset
        | WordBackwards -> editor.CaretOffset, findPrevWord editor isWordChar
        | WORDBackwards -> editor.CaretOffset, findPrevWord editor isWORDChar
        | BackwardToEndOfWord ->
            match findWordBackwards editor command.commandType isWordChar with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, 0
        | BackwardToEndOfWORD ->
            match findWordBackwards editor command.commandType isWORDChar with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, 0
        | ParagraphBackwards ->
            match paragraphBackwards editor with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, editor.CaretOffset
        | ParagraphForwards ->
            match paragraphForwards editor with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, editor.CaretOffset
        | InnerWord -> findCurrentWordStart editor isWordChar, findCurrentWordEnd editor isWordChar
        | AWord -> findCurrentWordStart editor isWordChar, findCurrentWordEnd editor isWordChar
        | ForwardToEndOfWord -> editor.CaretOffset, findWordEnd editor isWordChar
        | ForwardToEndOfWORD -> editor.CaretOffset, findWordEnd editor isWORDChar
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
        | SelectedText ->
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
    let registers = new Dictionary<Register, string>()
    registers.[EmptyRegister] <- ""

    let defaultState = { keys=[]; mode=NormalMode; visualStartOffset=0; findCharCommand=None; lastAction=[]; desiredColumn=None; undoGroup=None; statusMessage=None; markMap=System.Collections.Generic.Dictionary<string,MarkLocation>()}
    let (|VisualModes|_|) = function
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

    let (|MoveUpOrDown|_|) = function
        | { commandType=Move; textObject=Up }
        | { commandType=Move; textObject=Down } -> Some MoveUpOrDown
        | _ -> None

    let getCommand repeat commandType textObject =
        { repeat=repeat; commandType=commandType; textObject=textObject }

    let runOnce = getCommand (Some 1)
    let typeChar c = runOnce (InsertChar c) Nothing

    let runCommand vimState editor command =
        let delete state start finish =
            let finish =
                match command.textObject with
                | ForwardToEndOfWord
                | ForwardToEndOfWORD
                | EndOfLine
                | ToCharInclusive _
                | ToCharExclusive _ -> finish + 1
                | _ -> finish

            if command.textObject <> SelectedText then
                setSelection state editor command start finish
            registers.[EmptyRegister] <- editor.SelectedText
            EditActions.ClipboardCut editor
            state

        let setCaretMode state caretMode =
            match state.mode, caretMode with
            | NotInsertMode, Insert -> EditActions.SwitchCaretMode editor
            | InsertMode, Block -> EditActions.SwitchCaretMode editor
            | _ -> ()

        let switchToInsertMode (editor:TextEditor) state isInitial =
            let group = 
                if isInitial 
                    then editor.OpenUndoGroup() |> Some
                else
                    state.undoGroup
            setCaretMode state Insert
            { state with mode = InsertMode; statusMessage = "-- INSERT --" |> Some; keys = []; undoGroup = group }

        let toggleCase state start finish =
            if command.textObject <> SelectedText then
                setSelection state editor command start finish
            let toggleChar =
                function
                | c when Char.IsUpper c -> Char.ToLower c
                | c when Char.IsLower c -> Char.ToUpper c
                | c -> c
            match state.mode with
            | VisualBlockMode ->
                let selectionStartLocation = editor.OffsetToLocation vimState.visualStartOffset
                let topLine = Math.Min(selectionStartLocation.Line, editor.CaretLine) 
                let bottomLine = Math.Max(selectionStartLocation.Line, editor.CaretLine)
                editor.CaretColumn <- Math.Min(editor.CaretColumn, selectionStartLocation.Column)
                let offsets = [ topLine .. bottomLine ] |> List.map (fun c -> editor.LocationToOffset(c, editor.CaretColumn))
                for i in offsets do
                    let currentLetter = editor.[i]
                    let isLetter = Char.IsLetter editor.[i]
                    if isLetter then
                        let c = toggleChar currentLetter
                        editor.SetSelection(i, i+1)
                        EditActions.Delete editor
                        editor.InsertAtCaret (c.ToString())
                        EditActions.MoveCaretLeft editor
            | _ ->
                let swappedChars = editor.SelectedText |> Seq.map toggleChar |> Array.ofSeq
                EditActions.Delete editor
                editor.InsertAtCaret (swappedChars |> String)
            if command.textObject = SelectedText then
                editor.CaretOffset <- state.visualStartOffset
            state

        let modifyNumber f =
            let line = editor.GetLine editor.CaretLine
            let startOffset =
                let f = findCharBackwardsOnLineInclusive editor
                f line (fun c -> (c < '0' || c > '9') && c <> '-')
            let offset = 
                match startOffset with
                | Some i -> i+1
                | None -> editor.CaretOffset
            let lineToEnd = editor.Text.[offset .. line.EndOffset-1]
            let matches = Regex.Matches(lineToEnd, "-?[0-9]+", RegexOptions.Compiled) |> Seq.cast<Match>

            match matches |> Seq.tryHead with
            | Some m ->
                let i = Convert.ToInt32 (lineToEnd.[m.Index .. m.Index+m.Length-1])
                let replacement = (f i) |> string
                editor.ReplaceText(line.Offset+m.Index+(line.Length-lineToEnd.Length), m.Length, replacement)
                let line = editor.GetLine editor.CaretLine
                editor.CaretOffset <- line.Offset + m.Index+(line.Length-lineToEnd.Length)+m.Length-1
                vimState
            | None -> vimState

        let rec processCommands count vimState command isInitial =
            let blockInsert fColumnSelect =
                let selectionStartLocation = editor.OffsetToLocation vimState.visualStartOffset
                let topLine = min selectionStartLocation.Line editor.CaretLine
                let bottomLine = max selectionStartLocation.Line editor.CaretLine
                editor.CaretColumn <- fColumnSelect editor.CaretColumn selectionStartLocation.Column
                editor.SetSelection(new DocumentLocation (topLine, editor.CaretColumn),new DocumentLocation (bottomLine, editor.CaretColumn))
                if editor.SelectionMode = SelectionMode.Normal then dispatch TextEditorCommands.ToggleBlockSelectionMode
                switchToInsertMode editor vimState isInitial

            let start, finish = VimHelpers.getRange vimState editor command
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
                        match command, vimState.desiredColumn with
                        // don't change desired column if we already started moving up or down
                        | MoveUpOrDown, Some _c ->
                            editor.CaretOffset <- finish
                            vimState
                        | MoveUpOrDown, None ->
                            let res = { vimState with desiredColumn = Some editor.CaretColumn }
                            editor.CaretOffset <- finish
                            res
                        | _ ->
                            editor.CaretOffset <- finish
                            { vimState with desiredColumn = Some editor.CaretColumn }
                    newState

                | Delete -> 
                    let line = editor.GetLine editor.CaretLine
                    let finish =
                        if command.textObject = WordForwards && finish < line.EndOffset then
                            finish + 1
                        else
                            finish
                    let newState = delete vimState start finish
                    let line = editor.GetLine editor.CaretLine
                    let offsetBeforeDelimiter = if editor.CaretColumn < line.Length then editor.CaretOffset else editor.CaretOffset - 1
                    editor.CaretOffset <- Math.Max(offsetBeforeDelimiter, 0)
                    newState
                | ToggleCase ->
                    let newState = toggleCase vimState start finish
                    newState
                | DeleteWholeLines ->
                    let min = Math.Min(start, finish)
                    let max = Math.Max(start, finish)
                    let start = editor.GetLineByOffset(min).Offset
                    let finish = editor.GetLineByOffset(max).EndOffsetIncludingDelimiter
                    delete vimState start finish
                | DeleteLeft -> if editor.CaretColumn > 1 then delete vimState (editor.CaretOffset - 1) editor.CaretOffset else vimState
                | Change ->
                    let finish =
                        if count <> 1 && editor.[finish] = ' ' then
                            finish + 1
                        else
                            finish
                    let state =
                        if start <> finish then
                            delete vimState start finish
                        else
                            vimState
                    switchToInsertMode editor state isInitial
                | Yank register ->
                    let finish =
                        match command.textObject with
                        | ForwardToEndOfWord
                        | ForwardToEndOfWORD
                        | EndOfLine
                        | ToCharInclusive _
                        | ToCharExclusive _ -> finish + 1
                        | _ -> finish
                    if command.textObject <> SelectedText then
                        setSelection vimState editor command start finish
                    registers.[register] <- editor.SelectedText
                    if register = EmptyRegister then
                        EditActions.ClipboardCopy editor
                    editor.ClearSelection()
                    match vimState.mode with
                    | VisualModes -> editor.CaretOffset <- vimState.visualStartOffset
                    | _ -> ()
                    processCommands 1 vimState (runOnce (SwitchMode NormalMode) Nothing) false
                | Put Before ->
                    if registers.[EmptyRegister].EndsWith "\n" then
                        editor.CaretOffset <- editor.GetLine(editor.CaretLine).Offset
                        EditActions.ClipboardPaste editor
                        EditActions.MoveCaretUp editor
                    else
                        EditActions.ClipboardPaste editor
                    vimState
                | Put After ->
                    if registers.[EmptyRegister].EndsWith "\n" then
                        if editor.CaretLine = editor.LineCount then
                            let line = editor.GetLine(editor.CaretLine-1)
                            let delimiter = NewLine.GetString line.UnicodeNewline
                            editor.InsertText(editor.Text.Length, delimiter)
                            editor.CaretOffset <- editor.Text.Length
                            EditActions.ClipboardPaste editor
                            editor.RemoveText(editor.Text.Length-line.DelimiterLength, line.DelimiterLength)
                            EditActions.MoveCaretToLineStart editor
                        else
                            editor.CaretOffset <- editor.GetLine(editor.CaretLine).EndOffset+1
                            EditActions.ClipboardPaste editor
                            EditActions.MoveCaretUp editor
                    else
                        EditActions.MoveCaretRight editor
                        EditActions.ClipboardPaste editor
                        EditActions.MoveCaretLeft editor
                    vimState
                | Put OverSelection ->
                    EditActions.ClipboardPaste editor
                    { vimState with mode = NormalMode }
                | Visual ->
                    editor.SetSelection(start, finish); vimState
                | Undo -> EditActions.Undo editor; vimState
                | Redo -> EditActions.Redo editor; vimState
                | JoinLines ->
                    let lastColumn = editor.GetLine(editor.CaretLine).LengthIncludingDelimiter
                    EditActions.JoinLines editor
                    editor.CaretColumn <- lastColumn
                    vimState
                | ReplaceChar c ->
                    editor.SetSelection(editor.CaretOffset, editor.CaretOffset+1)
                    EditActions.Delete editor
                    editor.InsertAtCaret c
                    EditActions.MoveCaretLeft editor
                    vimState
                | SetMark c ->
                    if vimState.markMap.ContainsKey c then
                        vimState.markMap.Remove c |> ignore 
                    let location = { offset=editor.CaretOffset; fileName=editor.FileName.FullPath.ToString() }
                    vimState.markMap.Add (c, location) |> ignore
                    vimState
                | GoToMark c ->
                    if vimState.markMap.ContainsKey c then
                        let name = (vimState.markMap.Item c).fileName
                        if IdeApp.Workbench.ActiveDocument.FileName.FullPath.ToString() <> name then
                            let document = IdeApp.Workbench.GetDocument(name)
                            let fileInfo = new MonoDevelop.Ide.Gui.FileOpenInformation (document.FileName, document.Project)
                            IdeApp.Workbench.OpenDocument(fileInfo) |> ignore
                        editor.CaretOffset <- (vimState.markMap.Item c).offset
                    vimState
                | InsertLine Before -> 
                    EditActions.InsertNewLineAtEnd editor
                    vimState
                | InsertLine After -> 
                    if editor.CaretLine = 1 then
                        EditActions.MoveCaretToLineStart editor
                        EditActions.InsertNewLine editor
                        EditActions.MoveCaretUp editor
                        vimState
                    else
                        EditActions.MoveCaretUp editor
                        EditActions.InsertNewLineAtEnd editor
                        vimState
                | Dispatch command -> dispatch command ; vimState
                | ResetKeys -> { vimState with keys = [] }
                | BlockInsert Before -> blockInsert min
                | BlockInsert After -> blockInsert (fun s f -> (max s f) + 1)
                | SwitchMode mode ->
                    match mode with
                    | NormalMode ->
                        editor.ClearSelection()
                        setCaretMode vimState Block
                        vimState.undoGroup |> Option.iter(fun d -> d.Dispose())
                        { vimState with mode = mode; undoGroup = None; statusMessage = None }
                    | VisualMode | VisualLineMode | VisualBlockMode ->
                        setCaretMode vimState Block
                        let start, finish = VimHelpers.getRange vimState editor command
                        let statusMessage =
                            match mode with
                            | VisualMode -> Some "-- VISUAL --"
                            | VisualLineMode -> Some "-- VISUAL LINE --"
                            | VisualBlockMode -> Some "-- VISUAL BLOCK --"
                            | _ -> None
                        let newState = { vimState with mode = mode; visualStartOffset = editor.CaretOffset; statusMessage = statusMessage }
                        setSelection newState editor command start finish
                        match mode, editor.SelectionMode with
                        | VisualBlockMode, SelectionMode.Normal -> dispatch TextEditorCommands.ToggleBlockSelectionMode
                        | _, SelectionMode.Block -> dispatch TextEditorCommands.ToggleBlockSelectionMode
                        | _ -> ()
                        newState
                    | InsertMode ->
                        switchToInsertMode editor vimState isInitial
                | Star After ->
                    match wordAtCaret editor with
                    | Some word ->
                        let matches = Regex.Matches(editor.Text, sprintf @"\b%s\b" word)
                                      |> Seq.cast<Match>

                        let m =
                            matches
                            |> Seq.tryFind(fun m -> m.Index > editor.CaretOffset)

                        let offset =
                            match m with
                            | Some m -> m.Index
                            | None -> 
                                let m = matches |> Seq.head
                                m.Index
                        editor.CaretOffset <- offset
                        vimState
                    | None ->
                        processCommands 1 vimState (runOnce Move WordForwards) isInitial
                | Star Before ->
                    match wordAtCaret editor with
                    | Some word ->
                        let matches = Regex.Matches(editor.Text, sprintf @"\b%s\b" word)
                                      |> Seq.cast<Match>

                        let start = findCurrentWordStart editor isWordChar

                        let m =
                            matches
                            |> Seq.tryFindBack(fun m -> m.Index < start)

                        let offset =
                            match m with
                            | Some m -> m.Index
                            | None ->
                                let m = matches |> Seq.last
                                m.Index
                        editor.CaretOffset <- offset
                        vimState
                    | None -> vimState
                | InsertChar c ->
                    editor.InsertAtCaret c
                    vimState
                | IncrementNumber -> modifyNumber (fun i -> i + 1)
                | DecrementNumber -> modifyNumber (fun i -> i - 1)
                | _ -> vimState
            if count = 1 then newState else processCommands (count-1) newState command false
        let count = command.repeat |> Option.defaultValue 1

        use group = editor.OpenUndoGroup()

        processCommands count vimState command true

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

    let (|RegisterMatch|_|) = function
        | c -> Some (Register (Char.Parse c))

    let (|BlockDelimiter|_|) character =
        let pairs =
            [ 
                "[", ("[", "]")
                "]", ("[", "]")
                "(", ("(", ")")
                ")", ("(", ")")
                "b", ("(", ")")
                "{", ("{", "}")
                "}", ("{", "}")
                "B", ("{", "}")
                "<", ("<", ">")
                ">", ("<", ">")
                "\"", ("\"", "\"")
                "'", ("'", "'")
                "`", ("`", "`")
            ] |> dict
        if pairs.ContainsKey character then
            Some pairs.[character]
        else
            None

    let (|Movement|_|) = function
        | ["h"] -> Some Left
        | ["j"] -> Some Down
        | ["k"] -> Some Up
        | ["l"] -> Some Right
        | ["$"] -> Some EndOfLine
        | ["^"] -> Some FirstNonWhitespace
        | ["0"] -> Some StartOfLine
        | ["_"] -> Some FirstNonWhitespace
        | ["w"] -> Some WordForwards
        | ["W"] -> Some WORDForwards
        | ["b"] -> Some WordBackwards
        | ["B"] -> Some WORDBackwards
        | ["e"] -> Some ForwardToEndOfWord
        | ["E"] -> Some ForwardToEndOfWORD
        | ["g"; "e"] -> Some BackwardToEndOfWord
        | ["g"; "E"] -> Some BackwardToEndOfWORD
        | ["{"] -> Some ParagraphBackwards
        | ["}"] -> Some ParagraphForwards
        | ["%"] -> Some MatchingBrace
        | ["G"] -> Some LastLine
        | ["<C-d>"] -> Some HalfPageDown
        | ["<C-u>"] -> Some HalfPageUp
        | ["<C-f>"] -> Some PageDown
        | ["<C-b>"] -> Some PageUp
        | _ -> None

    let (|FindChar|_|) = function
        | "f" -> Some ToCharInclusive
        | "F" -> Some ToCharInclusiveBackwards
        | "t" -> Some ToCharExclusive
        | "T" -> Some ToCharExclusiveBackwards
        | _ -> None

    let (|Action|_|) = function
        | "d" -> Some Delete
        | "c" -> Some Change
        | "v" -> Some Visual
        | "y" -> Some (Yank EmptyRegister)
        | _ -> None

    let (|ModeChange|_|) = function
        | "i" -> Some InsertMode
        | "v" -> Some VisualMode
        | "<C-v>" -> Some VisualBlockMode
        | "V" -> Some VisualLineMode
        | _ -> None

    let (|Escape|_|) = function
        | "<esc>" | "<C-c>" | "<C-[>" -> Some Escape
        | _ -> None

    let (|MarkChar|_|) = function
        | "m" -> Some SetMark
        | "`" -> Some GoToMark
        | _ -> None

    let wait = [ getCommand None DoNothing Nothing ]

    let parseKeys (state:VimState) =
        let keyList = state.keys
        let numericArgument, keyList =
            match keyList with
            | "r" :: _ -> None, keyList
            | FindChar _ :: _ -> None, keyList
            | MarkChar _ :: _ -> None, keyList
            // 2dw -> 2, dw
            | OneToNine d1 :: Digit d2 :: Digit d3 :: Digit d4 :: t ->
                Some (d1 * 1000 + d2 * 100 + d3 * 10 + d4), t
            | OneToNine d1 :: Digit d2 :: Digit d3 :: t ->
                Some (d1 * 100 + d2 * 10 + d3), t
            | OneToNine d1 :: Digit d2 :: t ->
                Some (d1 * 10 + d2), t
            | OneToNine d :: t -> Some (d), t
            // d2w -> 2, dw
            | c :: OneToNine d1 :: Digit d2 :: Digit d3 :: Digit d4 :: t ->
                Some (d1 * 1000 + d2 * 100 + d3 * 10 + d4), c::t
            | c :: OneToNine d1 :: Digit d2 :: Digit d3 :: t ->
                Some (d1 * 100 + d2 * 10 + d3), c::t
            | c :: OneToNine d1 :: Digit d2 :: t ->
                Some (d1 * 10 + d2), c::t
            | c :: OneToNine d :: t ->
                Some d, c::t
            | _ -> None, keyList

        let run = getCommand numericArgument
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
            | VisualModes, [ Escape ] -> [ run (SwitchMode NormalMode) Nothing ]
            | _, [ Escape ] -> [ run (SwitchMode NormalMode) Nothing; run Move Left ]
            | NotInsertMode, [ "G" ] ->
                match numericArgument with
                | Some lineNumber -> [ runOnce Move (StartOfLineNumber lineNumber) ]
                | None -> [ runOnce Move LastLine ]
            | NormalMode, [ "V" ] ->
                match numericArgument with
                | Some lines -> [ run (SwitchMode VisualLineMode) Nothing; getCommand (lines-1 |> Some) Move Down ]
                | None -> [ run (SwitchMode VisualLineMode) Nothing ]
            | NormalMode, [ "v" ] ->
                match numericArgument with
                | Some chars -> [ runOnce (SwitchMode VisualMode) Nothing; getCommand (chars-1 |> Some) Move Right ]
                | None -> [ run (SwitchMode VisualMode) Nothing ]
            | NormalMode, [ "d"; "G" ] -> [ runOnce DeleteWholeLines LastLine]
            //| NormalMode, ["\"";œ r; "y"; "y" ] -> [ run (Yank (Register r.[0])) WholeLineIncludingDelimiter ]
            | NormalMode, [ "d"; "g" ] -> wait
            | NormalMode, [ "d"; "g"; "g" ] -> [ runOnce DeleteWholeLines StartOfDocument]
            | NotInsertMode, Movement m -> [ run Move m ]
            | NotInsertMode, [ FindChar m; c ] -> [ run Move (m c) ]
            | NormalMode, Action action :: Movement m -> [ run action m ]
            | NormalMode, [ "u" ] -> [ run Undo Nothing ]
            | NormalMode, [ "<C-r>" ] -> [ run Redo Nothing ]
            | NormalMode, [ "d"; "d" ] -> [ run Delete WholeLineIncludingDelimiter; run Move StartOfLine ]
            | NormalMode, [ "c"; "c" ] -> [ run Change WholeLine ]
            | NormalMode, ["\""] -> wait
            | NormalMode, ["\""; r ] -> wait
            | NormalMode, ["\""; r; "y"] -> wait
            | NormalMode, "\"" :: (RegisterMatch r) :: "y" :: (Movement m) -> [ run (Yank r) m]
            | NormalMode, [ "y"; "y" ]
            | NormalMode, [ "Y" ] -> 
                match numericArgument with
                | Some lines -> [ runOnce (SwitchMode VisualLineMode) Nothing; getCommand (lines-1 |> Some) Move Down; runOnce (Yank EmptyRegister) SelectedText ]
                | None -> [ runOnce (Yank EmptyRegister) WholeLineIncludingDelimiter ]
            | NormalMode, [ "C" ] -> [ run Change EndOfLine ]
            | NormalMode, [ "D" ] -> [ run Delete EndOfLine ]
            | NormalMode, [ "x" ] -> [ run Delete CurrentLocation ]
            | NormalMode, [ "X" ] -> [ run DeleteLeft Nothing ]
            | NormalMode, [ "s"] -> [ run Delete CurrentLocation; switchMode InsertMode ]
            | NormalMode, [ "p" ] -> [ run (Put After) Nothing ]
            | NormalMode, [ "P" ] -> [ run (Put Before) Nothing ]
            | VisualModes, [ "p" ] -> [ run (Put OverSelection) Nothing ]
            | VisualModes, [ "P" ] -> [ run (Put OverSelection) Nothing ]
            | NormalMode, [ "J" ] -> [ run JoinLines Nothing ]
            | NotInsertMode, [ "*" ] -> [ run (Star After) Nothing ]
            | NotInsertMode, [ "#" ] -> [ run (Star Before) Nothing ]
            | NotInsertMode, [ "£" ] -> [ run (Star Before) Nothing ]
            | NormalMode, [ "/" ] -> [ dispatch SearchCommands.Find ]
            | NormalMode, [ "n" ] -> [ dispatch SearchCommands.FindNext ]
            | NormalMode, [ "N" ] -> [ dispatch SearchCommands.FindPrevious ]
            | NormalMode, [ "z"; "z" ] -> [ dispatch TextEditorCommands.RecenterEditor ]
            | NormalMode, [ "z"; ] -> wait
            | NormalMode, [ "<C-y>" ] -> [ dispatch TextEditorCommands.ScrollLineUp ]
            | NormalMode, [ "<C-e>" ] -> [ dispatch TextEditorCommands.ScrollLineDown ]
            | NormalMode, [ "r" ] -> wait
            | NormalMode, [ "r"; c ] -> [ run (ReplaceChar c) Nothing ]
            | NormalMode, [ "m"; c ] -> [ run (SetMark c) Nothing ]
            | NormalMode, [ "`"; c ] -> [ run (GoToMark c) Nothing ]
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
            | NormalMode, [ "A" ] -> [ run Move EndOfLineIncludingDelimiter; switchMode InsertMode ]
            | NormalMode, [ "O" ] -> [ run (InsertLine After) Nothing; switchMode InsertMode ]
            | NormalMode, [ "o" ] -> [ run (InsertLine Before) Nothing; switchMode InsertMode ]
            | NormalMode, [ "I" ] -> [ run Move FirstNonWhitespace; switchMode InsertMode ]
            | NormalMode, [ Action _ ] -> wait
            | NotInsertMode, [ Action _; "i" ] -> wait
            | NotInsertMode, [ Action _; "a" ] -> wait
            | VisualMode, [ "i" ] | VisualMode, [ "a" ] -> wait
            | NotInsertMode, [ FindChar _; ] -> wait
            | NotInsertMode, [ Action _; FindChar _; ] -> wait
            | NotInsertMode, [ "g" ] -> wait
            | NotInsertMode, [ "m" ] -> wait
            | NotInsertMode, [ "`" ] -> wait
            | NotInsertMode, [ "g"; "g" ] ->
                let lineNumber = match numericArgument with Some n -> n | None -> 1
                [ runOnce Move (StartOfLineNumber lineNumber) ]
            | NotInsertMode, [ "g"; "d" ] -> [ dispatch "MonoDevelop.Refactoring.RefactoryCommands.GotoDeclaration" ]
            | NotInsertMode, [ "g"; "t" ] -> [ dispatch WindowCommands.NextDocument ]
            | NotInsertMode, [ "g"; "T" ] -> [ dispatch WindowCommands.PrevDocument ]
            | NotInsertMode, [ "." ] -> state.lastAction @ [ switchMode NormalMode ]
            | NotInsertMode, [ ";" ] -> match state.findCharCommand with Some command -> [ command ] | None -> []
            | VisualModes, Movement m -> [ run Move m ]
            | VisualBlockMode, [ "I" ] -> [ run (BlockInsert Before) Nothing ]
            | VisualBlockMode, [ "A" ] -> [ run (BlockInsert After) Nothing ]
            | VisualModes, [ "i"; BlockDelimiter c ] -> [ run Visual (InnerBlock c) ]
            | VisualModes, [ "a"; BlockDelimiter c ] -> [ run Visual (ABlock c) ]
            | VisualModes, [ "x" ] -> [ run Delete SelectedText; switchMode NormalMode ]
            | VisualModes, [ "d" ] -> [ run Delete SelectedText; switchMode NormalMode ]
            | VisualModes, [ "c" ] -> [ run Change SelectedText ]
            | NormalMode, [ "~" ] -> [ run ToggleCase CurrentLocation ]
            | VisualModes, [ "~" ] -> [ run ToggleCase SelectedText; switchMode NormalMode ]
            | VisualModes, [ "y" ] -> [ run (Yank EmptyRegister) SelectedText; switchMode NormalMode ]
            | VisualModes, [ "Y" ] -> [ run (Yank EmptyRegister) WholeLineIncludingDelimiter; switchMode NormalMode ]
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
            | NotInsertMode, [ "<C-a>" ] -> [ run IncrementNumber Nothing; switchMode NormalMode ]
            | NotInsertMode, [ "<C-x>" ] -> [ run DecrementNumber Nothing; switchMode NormalMode ]
            | _, [] when numericArgument.IsSome  -> wait
            | _ -> [ run ResetKeys Nothing ]
        action, newState

    let handleKeyPress state (keyPress:KeyDescriptor) editor =
        let newKeys, insertChar =
            match state.mode, keyPress.KeyChar with
            | _, c when keyPress.ModifierKeys = ModifierKeys.Control ->
                state.keys @ [sprintf "<C-%c>" c], None
            | NotInsertMode, c when keyPress.KeyChar <> '\000' ->
                state.keys @ [c |> string], None
            | _ ->
                match keyPress.SpecialKey with
                | SpecialKey.Escape -> ["<esc>"], None
                | SpecialKey.Left -> ["h"], None
                | SpecialKey.Down -> ["j"], None
                | SpecialKey.Up -> ["k"], None
                | SpecialKey.Right -> ["l"], None
                | _ -> state.keys, Some keyPress.KeyChar
        let newState = { state with keys = newKeys }
        let action, newState = parseKeys newState
        LoggingService.LogDebug (sprintf "%A" action)

        let rec performActions actions' state handled =
            match actions' with
            | [] -> state, handled
            | h::t ->
                if h.commandType = DoNothing then
                    newState, true
                else
                    let newState = runCommand state editor h

                    performActions t { newState with keys = [] } true

        let newState, handled = performActions action newState false

        let firstAction = action |> List.head

        let newState =
            match insertChar, firstAction.commandType, keyPress.KeyChar with
            | Some c, _, _ -> { newState with lastAction = newState.lastAction @ [ typeChar (c |> string) ]}
            | None, Delete, _
            | None, Change, _
            | None, ReplaceChar _, _
            | None, _, 'a'
            | None, _, 'i'
            | None, _, 'A' -> { newState with lastAction = action }
            | _ -> newState
        newState, handled

type XSVim() =
    inherit TextEditorExtension()
    static let editorStates = Dictionary<string, VimState>()
    let mutable disposables : IDisposable list = []
    let mutable processingKey = false
    member x.FileName = x.Editor.FileName.FullPath.ToString()

    override x.Initialize() =
        if not (editorStates.ContainsKey x.FileName) then
            editorStates.Add(x.FileName, Vim.defaultState )
            let editor = x.Editor
            EditActions.SwitchCaretMode editor
            let caretChanged =
                editor.CaretPositionChanged.Subscribe
                    (fun _e ->
                        if not processingKey then // only interested in mouse clicks
                            let line = editor.GetLine editor.CaretLine
                            if line.Length > 0 && editor.CaretColumn >= line.LengthIncludingDelimiter then
                                editor.CaretOffset <- editor.CaretOffset - 1)

            let documentClosed =
                IdeApp.Workbench.DocumentClosed.Subscribe
                    (fun e -> let documentName = e.Document.Name
                              if editorStates.ContainsKey documentName then
                                  editorStates.Remove documentName |> ignore)

            disposables <- [ caretChanged; documentClosed ]

    override x.KeyPress descriptor =
        match descriptor.ModifierKeys with
        | ModifierKeys.Command -> false
        | _ ->
            let vimState = editorStates.[x.FileName]
            let oldState = vimState

            processingKey <- true
            let newState, handledKeyPress = Vim.handleKeyPress vimState descriptor x.Editor
            processingKey <- false
            match newState.statusMessage with
            | Some m -> IdeApp.Workbench.StatusBar.ShowMessage m
            | _ -> IdeApp.Workbench.StatusBar.ShowReady()

            editorStates.[x.FileName] <- newState
            match oldState.mode with
            | InsertMode -> base.KeyPress descriptor
            | VisualMode -> false
            | _ -> not handledKeyPress

    override x.Dispose() =
        base.Dispose()
        disposables |> List.iter(fun d -> d.Dispose())

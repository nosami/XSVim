namespace XSVim

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open MonoDevelop.Components.Commands
open MonoDevelop.Core
open MonoDevelop.Core.Text
open MonoDevelop.Ide
open MonoDevelop.Ide.Commands
open MonoDevelop.Ide.Editor
open MonoDevelop.Ide.Editor.Extension
open Reflection

[<AutoOpen>]
module VimHelpers =
    let commandManager = IdeApp.CommandService |> Option.ofObj

    let dispatchCommand command = 
        commandManager 
        |> Option.iter(fun c -> c.DispatchCommand command |> ignore)

    let unregisterConflictingCommands() =
        match commandManager with
        | Some commandManager' ->
            let commands = commandManager'.GetCommands() |> Array.ofSeq
            [ 
                "Control+P"
                "Control+D"
                "Control+U"
                "Control+F"
                "Control+B"
                "Control+W"
                "Control+V"
                "Control+C"
                "Control+R"
                "Control+Y"
                "Control+E"
                "Control+A"
                "Control+X"
                "Control+N"
                "Control+O"
                "Control+I"
                "Control+["
            ] 
            |> List.iter(fun k -> 
                commands 
                |> Array.tryFind(fun command -> command.AccelKey = k)
                |> Option.iter commandManager'.UnregisterCommand)

        | None -> ()
    unregisterConflictingCommands()

    let closingBraces = [')'; '}'; ']'] |> set
    let openingbraces = ['('; '{'; '[' ] |> set

    let findNextBraceForwardsOnLine (editor:TextEditor) (line:IDocumentLine) =
        if closingBraces.Contains(editor.[editor.CaretOffset]) then
            Some editor.CaretOffset
        else
            seq { editor.CaretOffset .. line.EndOffset }
            |> Seq.tryFind(fun index -> openingbraces.Contains(editor.[index]))

    let findCharForwardsOnLine (editor:TextEditor) (line:IDocumentLine) startOffset character =
        let ch = char character
        seq { startOffset+1 .. line.EndOffset }
        |> Seq.tryFind(fun index -> editor.[index] = ch)

    let findCharBackwardsOnLine startOffset (editor:TextEditor) (line:IDocumentLine) matcher =
        seq { startOffset .. -1 .. line.Offset }
        |> Seq.tryFind (fun i -> matcher editor.[i])

    let findCharBackwardsOnLineExclusive (editor:TextEditor) startOffset = findCharBackwardsOnLine (startOffset-1) editor
    let findCharBackwardsOnLineInclusive (editor:TextEditor) = findCharBackwardsOnLine editor.CaretOffset editor

    let findStringCharBackwardsOnLine (editor:TextEditor) (line:IDocumentLine) startOffset character =
        let ch = char character
        let f = findCharBackwardsOnLineExclusive editor startOffset
        f line ((=) ch)

    let findCharForwards (editor:TextEditor) character =
        let ch = char character
        seq { editor.CaretOffset+1 .. editor.Text.Length-1 }
        |> Seq.tryFind(fun index -> editor.[index] = ch)

    let findCharBackwards (editor:TextEditor) character =
        let ch = char character
        seq { editor.CaretOffset .. -1 .. 0 }
        |> Seq.tryFind(fun index -> editor.[index] = ch)

    let rec findUnmatchedOpeningBrace(editor:TextEditor) pos openingChar closingChar =
        let find c =
            seq {pos .. -1 .. 0} |> Seq.tryFind(fun index -> editor.[index] = c)
        match find openingChar, find closingChar with
        | Some s, Some e when s > e -> Some s
        | Some s, Some e when s < e -> findUnmatchedOpeningBrace editor (s-1) openingChar closingChar 
        | Some s, None -> Some s
        | _,_ -> None

    let rec findUnmatchedClosingBrace(editor:TextEditor) pos openingChar closingChar =
        let find c =
            seq { pos+1 .. editor.Text.Length-1} |> Seq.tryFind(fun index -> editor.[index] = c)
        match find openingChar, find closingChar with
        | Some s, Some e when s > e -> Some e
        | Some s, Some e when s < e -> findUnmatchedClosingBrace editor e openingChar closingChar
        | _,_ -> None

    let isWordChar c = Char.IsLetterOrDigit c || c = '-' || c = '_'
    let isWORDChar c = not (Char.IsWhiteSpace c)
    let isNonBlankButNotWordChar c = isWORDChar c && not (isWordChar c)
    let isEOLChar c = c = '\r' || c = '\n'

    let (|WhiteSpace|_|) c =
        if Char.IsWhiteSpace c then Some WhiteSpace else None

    let (|EOLChar|_|) c =
        if isEOLChar c then Some EOLChar else None

    let findWordForwards (editor:TextEditor) commandType fWordChar =
        let findFromNonLetterChar index =
            match editor.[index], commandType with
            | EOLChar, Delete -> Some index
            | WhiteSpace, Move
            | WhiteSpace, Delete ->
                seq { index+1 .. editor.Text.Length-1 }
                |> Seq.tryFind(fun index -> not (editor.[index] = ' '))
                |> Option.bind(fun newIndex ->

                    let findFirstWordOnLine startOffset =
                        match editor.[startOffset] with
                        | WhiteSpace ->
                            seq { startOffset .. editor.Text.Length-1 }
                            |> Seq.tryFind(fun index -> let c = editor.[index]
                                                        fWordChar c || c = '\n')
                        | _ -> Some newIndex

                    match editor.[newIndex] with
                    | '\r' -> findFirstWordOnLine (newIndex + 2)
                    | '\n' -> findFirstWordOnLine (newIndex + 1)
                    | _ -> Some newIndex)
            | _ -> Some index

        if not (fWordChar editor.[editor.CaretOffset]) && fWordChar editor.[editor.CaretOffset + 1] then 
            editor.CaretOffset + 1 |> Some
        else
            seq { editor.CaretOffset+1 .. editor.Text.Length-1 }
            |> Seq.tryFind(fun index -> index = editor.Text.Length || not (fWordChar editor.[index]))
            |> Option.bind findFromNonLetterChar

    let findWordBackwards (editor:TextEditor) commandType fWordChar =
        let findFromNonLetterChar index =
            match editor.[index], commandType with
            | WhiteSpace, Move ->
                seq { index .. -1 .. 0 } 
                |> Seq.tryFind(fun index -> not (Char.IsWhiteSpace editor.[index]))
            | _ -> Some index

        if not (fWordChar editor.[editor.CaretOffset]) && fWordChar editor.[editor.CaretOffset - 1] then 
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

    let findCurrentWordEnd (editor:TextEditor) fWordChar =
        seq { editor.CaretOffset .. editor.Text.Length - 2 }
        |> Seq.tryFind(fun index -> not (fWordChar editor.[index+1]))
        |> Option.defaultValue (editor.Text.Length-1)

    let findWordEnd (editor:TextEditor) fWordChar =
        let currentWordEnd = findCurrentWordEnd editor fWordChar
        if editor.CaretOffset = currentWordEnd then
            let nextWordOffset = findWordForwards editor Move fWordChar
            match nextWordOffset with
            | Some offset ->
                let f =
                    if fWordChar editor.[offset] then fWordChar else isNonBlankButNotWordChar
                editor.CaretOffset <- offset
                findCurrentWordEnd editor f
            | None -> editor.Text.Length
        else
            currentWordEnd

    let findCurrentWordStart (editor:TextEditor) fWordChar =
        seq { editor.CaretOffset .. -1 .. 1 }
        |> Seq.tryFind(fun index -> not (fWordChar editor.[index-1]))
        |> Option.defaultValue 0

    let findNextWordStartOnLine(editor:TextEditor) (line:IDocumentLine) fWordChar =
        let currentWordEnd = findCurrentWordEnd editor fWordChar
        match editor.[currentWordEnd+1] with
        | WhiteSpace ->
            seq { currentWordEnd + 1 .. line.EndOffset }
            |> Seq.tryFind(fun index -> fWordChar editor.[index])
            |> Option.defaultValue line.EndOffset
        | _ -> currentWordEnd

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

    let getVisibleLines editor =
        let (lines:IDocumentLine seq) = editor?VisibleLines
        lines

    let getSortedVisibleLines editor =
        getVisibleLines editor |> Seq.sortBy(fun l -> l.LineNumber) /// the lines come back in random order

    let getVisibleLineCount editor =
        getVisibleLines editor |> Seq.length

    let getComparisonType (search:string) =
        match search with
        | s when s.ToLower() = s -> StringComparison.CurrentCultureIgnoreCase
        | _ -> StringComparison.CurrentCulture

    let findNextSearchOffset (editor:TextEditor) (search:string) (startOffset:int) =
        let comparison = getComparisonType search
        let index = editor.Text.IndexOf(search, startOffset, comparison)
        if index > -1 then
            Some index
        else
            let index = editor.Text.IndexOf(search, comparison)
            if index > -1 then Some index else None

    let findNextSearchOffsetBackwards (editor:TextEditor) (search:string) (startOffset:int) =
        let comparison = getComparisonType search
        let index = editor.Text.LastIndexOf(search, startOffset, comparison)
        if index > -1 then
            Some index
        else
            let index = editor.Text.LastIndexOf(search, editor.Length, comparison)
            if index > -1 then Some index else None

    let wordAtCaret (editor:TextEditor) =
        if isWordChar (editor.[editor.CaretOffset]) then
            let start = findCurrentWordStart editor isWordChar
            let finish = (findCurrentWordEnd editor isWordChar)
            let word = editor.GetTextAt(start, finish - start + 1)
            Some word
        else
            None

    let eofOnLine (line: IDocumentLine) = line.DelimiterLength = 0

    let findQuoteTriplet (editor:TextEditor) line quoteChar =
        let firstBackwards = findCharBackwardsOnLine editor.CaretOffset editor line ((=) quoteChar)
        let firstForwards = findCharForwardsOnLine editor line editor.CaretOffset (string quoteChar)
        let secondForwards =
            match firstForwards with
            | Some offset when offset + 1 < editor.Text.Length -> 
                findCharForwardsOnLine editor line offset (string quoteChar)
            | _ -> None
        firstBackwards, firstForwards, secondForwards

    let inferDelimiter (editor:TextEditor) =
        editor.GetLines()
        |> Seq.tryFind(fun line -> line.DelimiterLength > 0)
        |> Option.map(fun line -> match line.UnicodeNewline with
                                  | UnicodeNewline.LF -> "\n"
                                  | UnicodeNewline.CRLF -> "\r\n"
                                  | UnicodeNewline.CR -> "\r"
                                  | _ -> editor.Options.DefaultEolMarker)
        |> Option.defaultValue editor.Options.DefaultEolMarker

    let rec getRange (vimState:VimState) (editor:TextEditor) (command:VimAction) =
        let line = editor.GetLine editor.CaretLine
        match command.textObject with
        | Right behaviour ->
            let line = editor.GetLine editor.CaretLine
            let endOffset =
                match behaviour with
                | StopAtEndOfLine when editor.CaretColumn >= line.Length -> editor.CaretOffset
                | MoveToNextLineAtEnd when editor.[editor.CaretOffset+1] = '\r' -> editor.CaretOffset + 3
                | MoveToNextLineAtEnd when editor.[editor.CaretOffset+1] = '\n' -> editor.CaretOffset + 2
                | _ -> editor.CaretOffset + 1
            editor.CaretOffset, endOffset
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
        | Jump (StartOfLineNumber lineNumber) ->
            let line = editor.GetLine lineNumber
            editor.CaretOffset, line.Offset + editor.GetLineIndent(lineNumber).Length
        | Jump StartOfDocument -> editor.CaretOffset, 0
        | FirstNonWhitespace -> editor.CaretOffset, line.Offset + editor.GetLineIndent(editor.CaretLine).Length
        | WholeLine ->
            line.Offset, line.EndOffset
        | WholeLineIncludingDelimiter ->
            if eofOnLine line && line.LineNumber <> 1 then
                let delimiter = inferDelimiter editor
                line.Offset-delimiter.Length, line.EndOffsetIncludingDelimiter
            else
                line.Offset, line.EndOffsetIncludingDelimiter
        | Jump LastLine ->
            let lastLine = editor.GetLine editor.LineCount
            editor.CaretOffset, lastLine.Offset
        | Jump FirstVisibleLine ->
            let firstLine = getSortedVisibleLines editor |> Seq.head
            editor.CaretOffset, firstLine.Offset
        | Jump MiddleVisibleLine ->
            let firstLine = getSortedVisibleLines editor |> Seq.head
            let lastLine = getSortedVisibleLines editor |> Seq.last
            let middleLineNumber = (lastLine.LineNumber - firstLine.LineNumber) / 2 + firstLine.LineNumber
            let middleLine = editor.GetLine middleLineNumber
            editor.CaretOffset, middleLine.Offset
        | Jump LastVisibleLine ->
            let lastLine = getSortedVisibleLines editor |> Seq.last
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
            let opening = findUnmatchedOpeningBrace editor editor.CaretOffset (char startChar) (char endChar)
            let closing = findUnmatchedClosingBrace editor editor.CaretOffset (char startChar) (char endChar)
            match opening, closing with
            | Some start, Some finish -> start+1, finish
            | _, _ -> editor.CaretOffset, editor.CaretOffset
        | ABlock (startChar, endChar) ->
            let opening = findUnmatchedOpeningBrace editor editor.CaretOffset (char startChar) (char endChar)
            let closing = findUnmatchedClosingBrace editor editor.CaretOffset (char startChar) (char endChar)
            match opening, closing with
            | Some start, Some finish when finish < editor.Text.Length -> start, finish+1
            | _, _ -> editor.CaretOffset, editor.CaretOffset
        | InnerQuotedBlock c ->
            match findQuoteTriplet editor line c with
            | Some start, Some finish, _ -> start + 1, finish // we're inside quotes
            | None, Some start, Some finish -> start + 1, finish // there's quoted text to the right
            | _, _,_ -> editor.CaretOffset, editor.CaretOffset 
        | AQuotedBlock c ->
            match findQuoteTriplet editor line c with
            | Some start, Some finish, _ -> start, finish + 1 // we're inside quotes
            | None, Some start, Some finish -> start, finish + 1 // there's quoted text to the right
            | _, _,_ -> editor.CaretOffset, editor.CaretOffset 
        | WordForwards ->
            match findWordForwards editor command.commandType isWordChar with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, editor.Text.Length
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
        | Jump ParagraphBackwards ->
            match paragraphBackwards editor with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, editor.CaretOffset
        | Jump ParagraphForwards ->
            match paragraphForwards editor with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, editor.CaretOffset
        | InnerWord -> findCurrentWordStart editor isWordChar, findCurrentWordEnd editor isWordChar
        | InnerWORD -> findCurrentWordStart editor isWordChar, findNextWordStartOnLine editor line isWORDChar
        | AWord -> 
            if isWordChar (editor.[editor.CaretOffset]) then
                findCurrentWordStart editor isWordChar, findNextWordStartOnLine editor line isWORDChar 
            else
                let prevWordEnd = findWordBackwards editor Move isWordChar |> Option.defaultValue editor.CaretOffset
                let nextWordEnd = findWordEnd editor isWordChar
                prevWordEnd + 1, nextWordEnd + 1
        | ForwardToEndOfWord ->
            let isWordCharAtOffset offset = isWordChar (editor.[offset])

            match command.commandType, isWordCharAtOffset editor.CaretOffset, Char.IsWhiteSpace (editor.[editor.CaretOffset+1]) with
            | Change, true, true
            | Delete, true, true ->
                 editor.CaretOffset, editor.CaretOffset
            | _ -> editor.CaretOffset, findWordEnd editor isWordChar
        | ForwardToEndOfWORD -> editor.CaretOffset, findWordEnd editor isWORDChar
        | Jump HalfPageUp -> 
            let visibleLineCount = getVisibleLineCount editor
            let halfwayUp = max 1 (editor.CaretLine - visibleLineCount / 2)
            editor.CaretOffset, editor.GetLine(halfwayUp).Offset
        | Jump HalfPageDown -> 
            let visibleLineCount = getVisibleLineCount editor
            let halfwayDown = min editor.LineCount (editor.CaretLine + visibleLineCount / 2)
            editor.CaretOffset, editor.GetLine(halfwayDown).Offset
        | Jump PageUp ->
            let visibleLineCount = getVisibleLineCount editor
            let pageUp = max 1 (editor.CaretLine - visibleLineCount)
            editor.CaretOffset, editor.GetLine(pageUp).Offset
        | Jump PageDown ->
            let visibleLineCount = getVisibleLineCount editor
            let pageDown = min editor.LineCount (editor.CaretLine + visibleLineCount)
            editor.CaretOffset, editor.GetLine(pageDown).Offset
        | CurrentLocation -> editor.CaretOffset, editor.CaretOffset+1
        | SelectedText ->
            let selection = editor.Selections |> Seq.head
            let lead = editor.LocationToOffset selection.Lead
            let anchor = editor.LocationToOffset selection.Anchor
            Math.Min(lead, anchor), max lead anchor
        | SelectionStart -> editor.CaretOffset, vimState.visualStartOffset
        | MatchingBrace ->
            match findNextBraceForwardsOnLine editor line with
            | Some offset ->
                let startOffset = editor.CaretOffset
                editor.CaretOffset <- offset
                EditActions.GotoMatchingBrace editor
                startOffset, editor.CaretOffset
            | _ -> editor.CaretOffset, editor.CaretOffset
        | Jump (ToMark mark) ->
            if editor.FileName.FullPath.ToString() = mark.FileName then
                editor.CaretOffset, mark.Offset
            else 
                let document = IdeApp.Workbench.GetDocument(mark.FileName)
                let fileInfo = new MonoDevelop.Ide.Gui.FileOpenInformation (document.FileName, document.Project)
                IdeApp.Workbench.OpenDocument(fileInfo) |> ignore
                editor.CaretOffset, editor.CaretOffset
        | Offset offset -> editor.CaretOffset, offset
        | Jump (ToSearch search) ->
            let startOffset =
                match vimState.keys with
                | ["n"] | ["N"] -> editor.CaretOffset + 1
                | _ -> editor.CaretOffset
            let offset = findNextSearchOffset editor search startOffset |> Option.defaultValue editor.CaretOffset
            editor.CaretOffset, offset
        | Jump (ToSearchBackwards search) ->
            let offset = findNextSearchOffsetBackwards editor search editor.CaretOffset |> Option.defaultValue editor.CaretOffset
            editor.CaretOffset, offset
        | Jump SearchAgain ->
            match vimState.lastSearch with
            | Some search -> getRange vimState editor { command with textObject = search }
            | None -> editor.CaretOffset, editor.CaretOffset
        | Jump SearchAgainBackwards ->
            match vimState.lastSearch with
            | Some search ->
                let reverseSearch =
                    match search with
                    | Jump (ToSearch s) -> Jump (ToSearchBackwards s)
                    | Jump (ToSearchBackwards s) -> Jump (ToSearch s)
                    | _ -> failwith "Invalid search"
                getRange vimState editor { command with textObject = reverseSearch }
            | None -> editor.CaretOffset, editor.CaretOffset
        | _ -> editor.CaretOffset, editor.CaretOffset

module Vim =
    let registers = new Dictionary<Register, XSVim.Selection>()
    registers.[EmptyRegister] <- { linewise=false; content="" }

    let markDict = Dictionary<string, Marker>()
    let macros = Dictionary<char, VimAction list>()

    let (|VisualModes|_|) = function
        | VisualMode | VisualLineMode | VisualBlockMode -> Some VisualModes
        | _ -> None

    let (|NotInsertMode|_|) = function
        | InsertMode | ExMode _ -> None
        | _ -> Some NotInsertMode

    let setSelection vimState (editor:TextEditor) (command:VimAction) (start:int) finish =
        match vimState.mode, command.commandType with
        | VisualMode, Move | VisualMode, SwitchMode _ ->
            let start, finish =
                if finish < vimState.visualStartOffset then
                    finish, vimState.visualStartOffset + 1
                else
                    vimState.visualStartOffset, finish + if command.textObject = EndOfLine then 0 else 1
            editor.SetSelection(start, finish)
            if editor.SelectionMode = SelectionMode.Block then EditActions.ToggleBlockSelectionMode editor
        | VisualBlockMode, Move | VisualBlockMode, SwitchMode _ ->
            let selectionStartLocation = editor.OffsetToLocation vimState.visualStartOffset
            let leftColumn, rightColumn =
                if editor.CaretColumn < selectionStartLocation.Column then
                    editor.CaretColumn, selectionStartLocation.Column+1
                else
                    selectionStartLocation.Column, editor.CaretColumn+1
            let topLine = min selectionStartLocation.Line editor.CaretLine
            let bottomLine = max selectionStartLocation.Line editor.CaretLine
            editor.SetSelection(DocumentLocation (topLine, leftColumn), DocumentLocation (bottomLine, rightColumn))
            if editor.SelectionMode = SelectionMode.Normal then EditActions.ToggleBlockSelectionMode editor
        | VisualLineMode, Move | VisualLineMode, SwitchMode _ ->
            let startPos = min finish vimState.visualStartOffset
            let endPos = max finish vimState.visualStartOffset
            let startLine = editor.GetLineByOffset startPos
            let endLine = editor.GetLineByOffset endPos
            editor.SetSelection(startLine.Offset, endLine.EndOffsetIncludingDelimiter)
            if editor.SelectionMode = SelectionMode.Block then EditActions.ToggleBlockSelectionMode editor
        | _ -> editor.SetSelection(start, finish)

    let (|MoveUpOrDown|_|) = function
        | { commandType=Move; textObject=Up }
        | { commandType=Move; textObject=Down } -> Some MoveUpOrDown
        | _ -> None

    let (|LineWise|_|) = function
        | WholeLine
        | WholeLineIncludingDelimiter
        | Jump LastLine -> Some LineWise
        | _ -> None

    let (|StartsWithDelimiter|_|) (s:string) =
        if s.StartsWith "\r\n" then Some "\r\n"
        elif s.StartsWith "\n" then Some "\n"
        else None

    let (|EndsWithDelimiter|_|) (s:string) =
        if s.EndsWith "\r\n" then Some "\r\n"
        elif s.EndsWith "\n" then Some "\n"
        else None

    let isLineWise vimState command =
        match vimState.mode with
        | VisualLineMode -> true
        | _ ->
            match command.textObject with
            | LineWise -> true
            | _ -> false

    let getSelectedText vimState (editor: TextEditor) command =
        let linewise = isLineWise vimState command
        { linewise=linewise; content=editor.SelectedText }

    let setCaretMode editor state caretMode =
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

        setCaretMode editor state Insert
        { state with mode = InsertMode; statusMessage = "-- INSERT --" |> Some; keys = []; undoGroup = group }

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

            if start <> finish then
                if command.textObject <> SelectedText then
                    setSelection state editor command start finish
                registers.[EmptyRegister] <- getSelectedText state editor command
                EditActions.ClipboardCut editor
            state

        let setMark c =
            if markDict.ContainsKey c then
                let marker = markDict.[c]
                markDict.Remove c |> ignore 
                marker.Remove()
            let marker = Marker(editor, c)
            markDict.Add (c, marker) |> ignore

        let toggleCase state start finish =
            if command.textObject <> SelectedText then
                setSelection state editor command start finish

            let toggleChar = function
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
                        editor.InsertAtCaret (string c)
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
                editor.SetSelection(DocumentLocation (topLine, editor.CaretColumn), DocumentLocation (bottomLine, editor.CaretColumn))
                if editor.SelectionMode = SelectionMode.Normal then EditActions.ToggleBlockSelectionMode editor
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

                    match command.textObject with
                    | Jump _ ->
                        setMark "\'"
                        setMark "`"
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
                    let newState = delete vimState start finish
                    let offsetBeforeDelimiter =
                        match (isLineWise vimState command), command.textObject with
                        | true, _ -> editor.CaretOffset + editor.GetLineIndent(editor.CaretLine).Length
                        | false, _  when editor.CaretOffset < editor.Text.Length ->
                            if editor.[editor.CaretOffset] = '\n' || editor.[editor.CaretOffset] = '\r' then
                                editor.CaretOffset - 1
                            else
                                editor.CaretOffset
                        | _ -> editor.CaretOffset
                    editor.CaretOffset <- max offsetBeforeDelimiter 0
                    newState
                | Indent ->
                    let line = editor.GetLineByOffset(finish)
                    setSelection vimState editor command start line.EndOffset
                    EditActions.IndentSelection editor
                    editor.ClearSelection()
                    vimState
                | UnIndent ->
                    let line = editor.GetLineByOffset(finish)
                    setSelection vimState editor command start line.EndOffset
                    EditActions.UnIndentSelection editor
                    editor.ClearSelection()
                    vimState
                | Substitute ->
                    let newState = delete vimState start finish
                    switchToInsertMode editor newState isInitial
                | ToggleCase ->
                    let newState = toggleCase vimState start finish
                    newState
                | DeleteWholeLines ->
                    let min = min start finish
                    let max = max start finish
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
                    let state = delete vimState start finish
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
                    registers.[register] <- getSelectedText vimState editor command
                    if register = EmptyRegister then
                        EditActions.ClipboardCopy editor
                    editor.ClearSelection()
                    match vimState.mode with
                    | VisualModes -> editor.CaretOffset <- vimState.visualStartOffset
                    | _ -> ()
                    processCommands 1 vimState (runOnce (SwitchMode NormalMode) Nothing) false
                | Put Before ->
                    if registers.[EmptyRegister].linewise then
                        editor.CaretOffset <- editor.GetLine(editor.CaretLine).Offset
                        EditActions.ClipboardPaste editor
                        EditActions.MoveCaretUp editor
                        EditActions.MoveCaretToLineStart editor
                    else
                        EditActions.ClipboardPaste editor
                    vimState
                | Put After ->
                    if registers.[EmptyRegister].linewise then
                        if editor.CaretLine = editor.LineCount then
                            let line = editor.GetLine(editor.CaretLine)
                            let delimiter = inferDelimiter editor
                            match registers.[EmptyRegister].content with
                            | StartsWithDelimiter _delimiter -> ()
                            | _ -> editor.InsertText(editor.Text.Length, delimiter)
                            editor.CaretOffset <- editor.Text.Length
                            EditActions.ClipboardPaste editor
                            if eofOnLine line then
                                match registers.[EmptyRegister].content with
                                | EndsWithDelimiter delimiter -> editor.RemoveText(editor.Text.Length-delimiter.Length, delimiter.Length)
                                | _ -> ()
                            EditActions.MoveCaretToLineStart editor
                        else
                            editor.CaretOffset <- editor.GetLine(editor.CaretLine).EndOffsetIncludingDelimiter
                            EditActions.ClipboardPaste editor
                            EditActions.MoveCaretUp editor
                            EditActions.MoveCaretToLineStart editor
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
                | Undo -> EditActions.Undo editor; editor.ClearSelection(); vimState
                | Redo -> EditActions.Redo editor; vimState
                | JoinLines ->
                    let lastColumn = editor.GetLine(editor.CaretLine).Length
                    EditActions.JoinLines editor
                    editor.CaretColumn <- lastColumn + 1
                    vimState
                | ReplaceChar c ->
                    editor.SetSelection(editor.CaretOffset, editor.CaretOffset+1)
                    EditActions.Delete editor
                    match c with
                    | StartsWithDelimiter _ ->
                        EditActions.InsertNewLine editor
                    | _ ->
                        editor.InsertAtCaret c
                        EditActions.MoveCaretLeft editor
                    vimState
                | SetMark c ->
                    setMark c
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
                | Dispatch command -> dispatchCommand command ; vimState
                | ResetKeys -> { vimState with keys = [] }
                | BlockInsert Before -> blockInsert min
                | BlockInsert After -> blockInsert (fun s f -> (max s f) + 1)
                | SwitchMode mode ->
                    match mode with
                    | NormalMode ->
                        let lastSelection =
                            match vimState.mode with
                            | VisualModes ->
                                Some { start = vimState.visualStartOffset; finish = editor.CaretOffset; mode = vimState.mode }
                            | _ -> vimState.lastSelection
                        editor.ClearSelection()
                        setCaretMode editor vimState Block
                        // stupid hack to prevent intellisense in normal mode
                        // https://github.com/mono/monodevelop/blob/fdbfbe89529bd9076e1906e7b70fdb51a9ae6b99/main/src/core/MonoDevelop.Ide/MonoDevelop.Ide.Editor.Extension/CompletionTextEditorExtension.cs#L153
                        if editor.SelectionMode = SelectionMode.Normal then EditActions.ToggleBlockSelectionMode editor
                        vimState.undoGroup |> Option.iter(fun d -> d.Dispose())
                        let vimState =
                            if vimState.mode = InsertMode then
                                processCommands 1 vimState (runOnce (SetMark ".") Nothing) false
                            else
                                vimState
                        { vimState with mode = mode; lastSelection = lastSelection; undoGroup = None; statusMessage = None }
                    | VisualMode | VisualLineMode | VisualBlockMode ->
                        setCaretMode editor vimState Block
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
                        | VisualBlockMode, SelectionMode.Normal -> EditActions.ToggleBlockSelectionMode editor
                        | _, SelectionMode.Block -> EditActions.ToggleBlockSelectionMode editor
                        | _ -> ()
                        newState
                    | InsertMode ->
                        switchToInsertMode editor vimState isInitial
                    | ExMode c ->
                        { vimState with mode = (ExMode c); statusMessage = string c |> Some }
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
                    match vimState.mode with
                    | InsertMode ->
                        editor.InsertAtCaret c
                        vimState
                    | _ -> vimState
                | IncrementNumber -> modifyNumber (fun i -> i + 1)
                | DecrementNumber -> modifyNumber (fun i -> i - 1)
                | IncrementalSearch search ->
                    findNextSearchOffset editor search editor.CaretOffset
                    |> Option.iter(fun index ->
                                       editor.SetSelection(index, index + search.Length)
                                       editor.ScrollTo(index))
                    vimState
                | IncrementalSearchBackwards search ->
                    findNextSearchOffsetBackwards editor search editor.CaretOffset
                    |> Option.iter(fun index ->
                                       editor.SetSelection(index, index + search.Length)
                                       editor.ScrollTo(index))
                    vimState
                | SetSearchAction command -> { vimState with searchAction = Some command }
                | MacroStart c ->
                    macros.[c] <- []
                    { vimState with macro = Macro (char c) |> Some }
                | MacroEnd ->
                    { vimState with macro = None }
                | ReplayMacro c ->
                    let getCount repeat = repeat |> Option.defaultValue 1

                    let rec runMacro state actions =
                        match actions with
                        | [ only ] ->
                            processCommands (getCount only.repeat) state only false 
                        | h :: t ->
                            let newState = processCommands (getCount h.repeat) state h false 
                            runMacro newState t
                        | [] -> state
                    runMacro vimState macros.[c] 
                | NextTab ->
                    Window.nextTab editor
                    vimState
                | PreviousTab ->
                    Window.previousTab editor
                    vimState
                | Func f -> 
                    f editor
                    vimState
                | ChangeState s -> s
                | _ -> vimState
            if count = 1 then newState else processCommands (count-1) newState command false
        let count = command.repeat |> Option.defaultValue 1

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
            ] |> dict
        if pairs.ContainsKey character then
            Some pairs.[character]
        else
            None

    let (|QuoteDelimiter|_|) character =
        if Array.contains character [| "\""; "'"; "`"|] then
            Some character
        else
            None

    let (|Movement|_|) = function
        | ["h"] -> Some Left
        | ["j"] -> Some Down
        | ["k"] -> Some Up
        | ["l"] -> Some (Right StopAtEndOfLine)
        | [" "] -> Some (Right MoveToNextLineAtEnd)
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
        | ["{"] -> Some (Jump ParagraphBackwards)
        | ["}"] -> Some (Jump ParagraphForwards)
        | ["%"] -> Some MatchingBrace
        | ["G"] -> Some (Jump LastLine)
        | ["H"] -> Some (Jump FirstVisibleLine)
        | ["M"] -> Some (Jump MiddleVisibleLine)
        | ["L"] -> Some (Jump LastVisibleLine)
        | ["<C-d>"] -> Some (Jump HalfPageDown)
        | ["<C-u>"] -> Some (Jump HalfPageUp)
        | ["<C-f>"] -> Some (Jump PageDown)
        | ["<C-b>"] -> Some (Jump PageUp)
        | ["n"] -> Some (Jump SearchAgain)
        | ["N"] -> Some (Jump SearchAgainBackwards)
        | _ -> None

     
    let (|IndentChar|_|) = function
        | ">" -> Some Indent
        | "<" -> Some UnIndent
        | _ -> None

    let (|FindChar|_|) = function
        | "f" -> Some ToCharInclusive
        | "F" -> Some ToCharInclusiveBackwards
        | "t" -> Some ToCharExclusive
        | "T" -> Some ToCharExclusiveBackwards
        | _ -> None

    let (|SearchChar|_|) = function
        | "/" -> Some (SearchChar '/')
        | "?" -> Some (SearchChar '?')
        | _ -> None

    let (|Action|_|) = function
        | "d" -> Some Delete
        | "c" -> Some Change
        | "v" -> Some Visual
        | "y" -> Some (Yank EmptyRegister)
        | ">" -> Some Indent
        | "<" -> Some UnIndent
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

    let parseKeys (state:VimState) =
        let keyList = state.keys
        let numericArgument, keyList =
            match keyList with
            | "r" :: _ -> None, keyList
            | FindChar _ :: _ -> None, keyList
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

        LoggingService.LogDebug (sprintf "%A %A" state.mode keyList)
        let newState =
            match keyList with
            | [ FindChar m; c ] -> { state with findCharCommand = run Move ( m c ) |> Some }
            | _ -> state

        let action =
            match state.mode, keyList with
            | VisualBlockMode, [ Escape ] -> [ switchMode NormalMode; run Move SelectionStart ]
            | NormalMode, [ Escape ] -> resetKeys
            | VisualModes, [ Escape ] -> [ switchMode NormalMode ]
            | ExMode _, [ Escape ] -> [ switchMode NormalMode ]
            | _, [ Escape ] -> [ switchMode NormalMode; run Move Left ]
            | NotInsertMode, [ "G" ] ->
                match numericArgument with
                | Some lineNumber -> [ runOnce Move (Jump (StartOfLineNumber lineNumber)) ]
                | None -> [ runOnce Move (Jump LastLine) ]
            | NormalMode, [ IndentChar _ ] -> wait
            | NormalMode, [ IndentChar _ ; "g" ] -> wait
            | NormalMode, [ IndentChar indent; "G" ] ->
                match numericArgument with
                | Some lineNumber -> [ runOnce Indent (Jump (StartOfLineNumber lineNumber)) ]
                | None -> [ runOnce indent (Jump LastLine) ]
            | NormalMode, [ IndentChar indent; "g"; "g" ] ->
                let lineNumber = match numericArgument with Some n -> n | None -> 1
                [ runOnce indent (Jump (StartOfLineNumber lineNumber)) ]
            | NormalMode, [ ">"; ">" ] -> [ run Indent WholeLine ]
            | NormalMode, [ "<"; "<" ] -> [ run UnIndent WholeLine ]
            | NormalMode, [ "V" ] ->
                match numericArgument with
                | Some lines -> [ switchMode VisualLineMode; getCommand (lines-1 |> Some) Move Down ]
                | None -> [ switchMode VisualLineMode ]
            | NormalMode, [ "v" ] ->
                match numericArgument with
                | Some chars -> [ switchMode VisualMode; getCommand (chars-1 |> Some) Move (Right StopAtEndOfLine) ]
                | None -> [ switchMode VisualMode ]
            | NormalMode, [ "d"; "G" ] -> [ runOnce DeleteWholeLines (Jump LastLine)]
            | NormalMode, [ "d"; "g" ] -> wait
            | NormalMode, [ "d"; "g"; "g" ] -> [ runOnce DeleteWholeLines (Jump StartOfDocument)]
            | NotInsertMode, Movement m -> [ run Move m ]
            | NotInsertMode, [ FindChar m; c ] -> [ run Move (m c) ]
            | NormalMode, IndentChar indent :: Movement m ->
                match numericArgument with
                | None -> [ run indent m ]
                | Some repeat ->
                    let movements =
                        [ 1 .. repeat ]
                        |> List.map (fun _ -> runOnce Move m)

                    [ yield switchMode VisualMode
                      yield! movements
                      yield runOnce indent SelectedText
                      yield switchMode NormalMode ]

            | NormalMode, Action action :: Movement m -> [ run action m ]
            | NormalMode, [ "u" ] -> [ run Undo Nothing ]
            | NormalMode, [ "<C-r>" ] -> [ run Redo Nothing ]
            | NormalMode, [ "d"; "d" ] -> [ run Delete WholeLineIncludingDelimiter; run Move FirstNonWhitespace ]
            | NormalMode, [ "c"; "c" ] -> [ run Change WholeLine ]
            | NormalMode, ["\""] -> wait
            | NormalMode, ["\""; _ ] -> wait
            | NormalMode, ["\""; _; "y"] -> wait
            | NormalMode, "\"" :: (RegisterMatch r) :: "y" :: (Movement m) -> [ run (Yank r) m]
            | NormalMode, [ "y"; "y" ]
            | NormalMode, [ "Y" ] -> 
                match numericArgument with
                | Some lines -> [ switchMode VisualLineMode; getCommand (lines-1 |> Some) Move Down; runOnce (Yank EmptyRegister) SelectedText ]
                | None -> [ runOnce (Yank EmptyRegister) WholeLineIncludingDelimiter ]
            | NormalMode, [ "C" ] -> [ run Change EndOfLine ]
            | NormalMode, [ "D" ] -> [ run Delete EndOfLine ]
            | NormalMode, [ "x" ] -> [ run Delete CurrentLocation ]
            | NormalMode, [ "X" ] -> [ run DeleteLeft Nothing ]
            | NormalMode, [ "s"] -> [ run Substitute CurrentLocation]
            | NormalMode, [ "S"] -> [ run Delete WholeLineIncludingDelimiter; runOnce (InsertLine After) Nothing; switchMode InsertMode ]
            | NormalMode, [ "p" ] -> [ run (Put After) Nothing ]
            | NormalMode, [ "P" ] -> [ run (Put Before) Nothing ]
            | VisualModes, [ "p" ] -> [ run (Put OverSelection) Nothing ]
            | VisualModes, [ "P" ] -> [ run (Put OverSelection) Nothing ]
            | NormalMode, [ "J" ] -> [ run JoinLines Nothing ]
            | NotInsertMode, [ "*" ] -> [ run (Star After) Nothing ]
            | NotInsertMode, [ "#" ] -> [ run (Star Before) Nothing ]
            | NotInsertMode, [ "£" ] -> [ run (Star Before) Nothing ]
            | NotInsertMode, [ SearchChar c ] -> [ switchMode (ExMode c); runOnce (SetSearchAction Move) Nothing ]
            | NotInsertMode, [ ":" ] -> [ switchMode (ExMode ':') ]
            | NotInsertMode, [ Action action; SearchChar c ] -> [ switchMode (ExMode c); runOnce (SetSearchAction action) Nothing ]
            | NormalMode, [ "z"; "z" ] -> [ dispatch TextEditorCommands.RecenterEditor ]
            | NormalMode, [ "z"; ] -> wait
            | NormalMode, [ "<C-y>" ] -> [ dispatch TextEditorCommands.ScrollLineUp ]
            | NormalMode, [ "<C-e>" ] -> [ dispatch TextEditorCommands.ScrollLineDown ]
            | NormalMode, [ "<C-o>" ] -> [ dispatch NavigationCommands.NavigateBack ]
            | NormalMode, [ "<C-i>" ] -> [ dispatch NavigationCommands.NavigateForward ]
            | NormalMode, [ "r" ] -> wait
            | NormalMode, [ "r"; "<ret>" ] -> [ run (ReplaceChar "\n" ) Nothing ]
            | NormalMode, [ "r"; c ] -> [ run (ReplaceChar c) Nothing ]
            | NormalMode, [ "m"; c ] -> [ run (SetMark c) Nothing ]
            | NotInsertMode, [ "`"; c] -> 
                match markDict.TryGetValue c with
                | true, mark -> [ runOnce Move (Jump (ToMark mark))]
                | _ -> [ run ResetKeys Nothing]
            | NotInsertMode, [ "'"; c] -> 
                match markDict.TryGetValue c with
                | true, mark -> [ runOnce Move (Jump (ToMark mark)); runOnce Move FirstNonWhitespace ]
                | _ -> [ run ResetKeys Nothing]
            | NotInsertMode, [ Action action; FindChar m; c ] -> [ run action (m c) ]
            | NotInsertMode, [ Action action; FindChar m; c ] -> [ run action (m c) ]
            | NotInsertMode, [ Action action; "i"; BlockDelimiter c ] -> [ run action (InnerBlock c) ]
            | NotInsertMode, [ Action action; "a"; BlockDelimiter c ] -> [ run action (ABlock c) ]
            | NotInsertMode, [ Action action; "i"; QuoteDelimiter c ] -> [ run action (InnerQuotedBlock (char c)) ]
            | NotInsertMode, [ Action action; "a"; QuoteDelimiter c ] -> [ run action (AQuotedBlock (char c)) ]
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
            | NotInsertMode, [ "<ret>" ] -> [ run Move Down; run Move FirstNonWhitespace ]
            | NotInsertMode, [ "q" ] when state.macro.IsNone -> wait
            | NotInsertMode, [ "q"; c ] -> [ run (MacroStart (char c)) Nothing ]
            | NotInsertMode, [ "q" ] -> [ run MacroEnd Nothing ]
            | NotInsertMode, [ "@" ] -> wait
            | NotInsertMode, [ "@"; c ] -> [ run (ReplayMacro (char c)) Nothing ]
            | NotInsertMode, [ "g" ] -> wait
            | NotInsertMode, [ "m" ] -> wait
            | NotInsertMode, [ "`" ] -> wait
            | NotInsertMode, [ "'" ] -> wait
            | NotInsertMode, [ "g"; "g" ] ->
                let lineNumber = match numericArgument with Some n -> n | None -> 1
                [ runOnce Move (Jump (StartOfLineNumber lineNumber)) ]
            | NotInsertMode, [ "g"; "d" ] -> [ dispatch "MonoDevelop.Refactoring.RefactoryCommands.GotoDeclaration" ]
            | NotInsertMode, [ "g"; "t" ] -> [ func Window.nextTab ]
            | NotInsertMode, [ "g"; "T" ] -> [ func Window.previousTab ]
            | NotInsertMode, [ "g"; "v" ] ->
                match state.lastSelection with
                | Some selection -> [ run Move (Offset selection.start)
                                      switchMode selection.mode
                                      run Move (Offset selection.finish) ]
                | None -> resetKeys
            | NotInsertMode, [ "." ] -> state.lastAction @ [ switchMode NormalMode ]
            | NotInsertMode, [ ";" ] -> match state.findCharCommand with Some command -> [ command ] | None -> []
            | NotInsertMode, [ "," ] ->
                match state.findCharCommand with
                | Some command -> 
                    let findCommand =
                        match command.textObject with
                        | ToCharInclusive c -> ToCharInclusiveBackwards c
                        | ToCharInclusiveBackwards c -> ToCharInclusive c
                        | ToCharExclusive c -> ToCharExclusiveBackwards c
                        | ToCharExclusiveBackwards c -> ToCharExclusive c
                        | _ -> failwith "Invalid find command"
                    [ { command with textObject=findCommand } ] 
                | None -> []
            | VisualModes, Movement m -> [ run Move m ]
            | VisualBlockMode, [ "I" ] -> [ run (BlockInsert Before) Nothing ]
            | VisualBlockMode, [ "A" ] -> [ run (BlockInsert After) Nothing ]
            | VisualModes, [ "i"; BlockDelimiter c ] -> [ run Visual (InnerBlock c) ]
            | VisualModes, [ "a"; BlockDelimiter c ] -> [ run Visual (ABlock c) ]
            | VisualModes, [ "i"; QuoteDelimiter c ] -> [ run Visual (InnerQuotedBlock (char c)) ]
            | VisualModes, [ "a"; QuoteDelimiter c ] -> [ run Visual (AQuotedBlock (char c)) ]
            | VisualModes, [ "x" ] -> [ run Delete SelectedText; switchMode NormalMode ]
            | VisualModes, [ "d" ] -> [ run Delete SelectedText; switchMode NormalMode ]
            | VisualModes, [ "c" ] -> [ run Change SelectedText ]
            | NormalMode, [ "~" ] -> [ run ToggleCase CurrentLocation ]
            | VisualModes, [ "~" ] -> [ run ToggleCase SelectedText; switchMode NormalMode ]
            | VisualModes, [ "y" ] -> [ run (Yank EmptyRegister) SelectedText; switchMode NormalMode ]
            | VisualModes, [ "Y" ] -> [ run (Yank EmptyRegister) WholeLineIncludingDelimiter; switchMode NormalMode ]
            | VisualModes, [ ">" ] -> [ run (Func EditActions.IndentSelection) Nothing; switchMode NormalMode ]
            | VisualModes, [ "<" ] -> [ run (Func EditActions.UnIndentSelection) Nothing; switchMode NormalMode ]
            | NotInsertMode, [ "<C-p>" ] -> [ dispatch SearchCommands.GotoFile ]
            | NotInsertMode, [ "<C-w>" ] -> wait
            | NotInsertMode, [ "<C-w>"; "w" ]
            | NotInsertMode, [ "<C-w>"; "<C-w>" ] -> [ func Window.switchWindow ]
            | NotInsertMode, [ "<C-w>"; "h" ] -> [ func Window.leftWindow ]
            | NotInsertMode, [ "<C-w>"; "l" ] -> [ func Window.rightWindow ]
            // These commands don't work the same way as vim yet, but better than nothing
            | NotInsertMode, [ "<C-w>"; "o" ] -> [ dispatch FileTabCommands.CloseAllButThis ]
            | NotInsertMode, [ "<C-w>"; "c" ] -> [ func Window.closeTab ]
            | NotInsertMode, [ "<C-w>"; "v" ]
            | NotInsertMode, [ "<C-w>"; "s" ] 
            | NotInsertMode, [ "<C-w>"; "<C-v>" ]
            | NotInsertMode, [ "<C-w>"; "<C-s>" ] ->
                let notebooks = Window.getNotebooks()
                if notebooks.Length < 2 then
                    [ dispatch "MonoDevelop.Ide.Commands.ViewCommands.SideBySideMode" ]
                else
                    resetKeys
            | InsertMode, [ "<C-n>" ] -> [ dispatch TextEditorCommands.DynamicAbbrev ]
            | NotInsertMode, [ "<C-a>" ] -> [ run IncrementNumber Nothing; switchMode NormalMode ]
            | NotInsertMode, [ "<C-x>" ] -> [ run DecrementNumber Nothing; switchMode NormalMode ]
            | _, [] when numericArgument.IsSome  -> wait
            | _ -> resetKeys
        action, newState

    let handleKeyPress state (keyPress:KeyDescriptor) editor =
        let newKeys, insertChar =
            match state.mode, keyPress.KeyChar with
            | _, c when keyPress.ModifierKeys = ModifierKeys.Control ->
                state.keys @ [sprintf "<C-%c>" c], None
            | _, c when keyPress.ModifierKeys = ModifierKeys.Command ->
                state.keys @ [sprintf "<D-%c>" c], None
            | _, 'z' when keyPress.ModifierKeys = ModifierKeys.Command ->
                state.keys @ ["u"], None
            | NotInsertMode, c when keyPress.KeyChar <> '\000' ->
                state.keys @ [c |> string], None
            | _ ->
                match keyPress.SpecialKey with
                | SpecialKey.Escape -> state.keys @ ["<esc>"], None
                | SpecialKey.Return -> state.keys @ ["<ret>"], None
                | SpecialKey.Left -> state.keys @ ["h"], None
                | SpecialKey.Down -> state.keys @ ["j"], None
                | SpecialKey.Up -> state.keys @ ["k"], None
                | SpecialKey.Right -> state.keys @ ["l"], None
                | _ -> state.keys, Some keyPress.KeyChar
        let newState = { state with keys = newKeys }
        let action, newState = parseKeys newState
        let newState =
            match state.statusMessage, state.mode, newState.mode with
            | Some _, NormalMode, NormalMode -> { newState with statusMessage = None }
            | _ -> newState

        LoggingService.LogDebug (sprintf "%A" action)

        let rec performActions actions' state handled =
            match actions' with
            | [] -> state, handled
            | [ only ] ->
                match only.commandType with
                | DoNothing -> state, true
                | _ ->
                    let newState = runCommand state editor only
                    { newState with keys = [] }, true
            | h::t ->
                let newState = runCommand state editor h
                performActions t newState true

        let newState, handled =
            match state.mode with
            | ExMode _ ->
                let state, actions = exMode.processKey state keyPress
                performActions actions state true
            | _ ->
                use group = editor.OpenUndoGroup()
                state.macro
                |> Option.iter(fun (Macro c) -> macros.[c] <- macros.[c] @ action)
                performActions action newState false

        let firstAction = action |> List.head

        let newState =
            match insertChar, firstAction.commandType, keyPress.KeyChar with
            | Some c, _, _ ->
                newState.macro |> Option.iter(fun (Macro m) ->
                    macros.[m] <- macros.[m] @ [ typeChar (c |> string) ])
                { newState with lastAction = newState.lastAction @ [ typeChar (c |> string) ]}
            | None, Delete, _
            | None, Change, _
            | None, Indent, _
            | None, UnIndent, _
            | None, Put _, _
            | None, ReplaceChar _, _
            | None, _, 'a'
            | None, _, 'i'
            | None, _, 'o'
            | None, _, 'O'
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
        treeViewPads.initialize()

        if not (editorStates.ContainsKey x.FileName) then
            editorStates.Add(x.FileName, VimState.Default)
            let editor = x.Editor
            editor.GrabFocus()
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
        | ModifierKeys.Control
        | ModifierKeys.Command when descriptor.KeyChar = 'z' ->
            // cmd-z uses the vim undo group
            let vimState = editorStates.[x.FileName]
            vimState.undoGroup |> Option.iter(fun d -> d.Dispose())
            EditActions.Undo x.Editor
            x.Editor.ClearSelection()
            false
        | ModifierKeys.Command when descriptor.KeyChar <> 'z' && descriptor.KeyChar <> 'r' -> false
        | _ ->
            let vimState = editorStates.[x.FileName]
            let oldState = vimState

            processingKey <- true
            let newState, handledKeyPress = Vim.handleKeyPress vimState descriptor x.Editor
            processingKey <- false

            match newState.statusMessage, newState.macro with
            | Some m, None -> IdeApp.Workbench.StatusBar.ShowMessage m
            | Some m, Some _ -> IdeApp.Workbench.StatusBar.ShowMessage (m + "recording")
            | None, Some _ -> IdeApp.Workbench.StatusBar.ShowMessage "recording"
            | _ -> IdeApp.Workbench.StatusBar.ShowReady()

            editorStates.[x.FileName] <- newState
            match oldState.mode, newState.mode with
            | InsertMode, InsertMode ->
                base.KeyPress descriptor
            | VisualMode, _ -> false
            | _ -> not handledKeyPress

    [<CommandUpdateHandler ("MonoDevelop.Ide.Commands.EditCommands.Undo")>]
    // We handle cmd-z ourselves to use the vim undo stack
    member x.CanUndo(ci:CommandInfo) = ci.Enabled <- false

    [<CommandUpdateHandler ("MonoDevelop.Ide.Commands.EditCommands.Rename")>]
    member x.Rename(ci:CommandInfo) =
        ci.Enabled <- true
        // dirty hack - use the command update handler to switch to insert mode
        // before the inline rename kicks in
        let state = editorStates.[x.FileName]
        if state.mode <> InsertMode then
            editorStates.[x.FileName] <-
                Vim.switchToInsertMode x.Editor state false

    override x.Dispose() =
        base.Dispose()
        disposables |> List.iter(fun d -> d.Dispose())

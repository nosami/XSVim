namespace XSVim

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open System.Threading
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

    let closingBraces = [')'; '}'; ']'] |> set
    let openingbraces = ['('; '{'; '[' ] |> set

    let markDict = Dictionary<string, Marker>()

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
        seq { editor.CaretOffset+1 .. editor.Length-1 }
        |> Seq.tryFind(fun index -> editor.[index] = ch)

    let findCharBackwards (editor:TextEditor) character =
        let ch = char character
        seq { editor.CaretOffset .. -1 .. 0 }
        |> Seq.tryFind(fun index -> editor.[index] = ch)

    let findUnmatchedBlockDelimiter(editor:TextEditor) pos blockStartDelimiter blockEndDelimiter op =
        let blockStartShift = (blockStartDelimiter |> String.length) - 1
        let blockEndShift = (blockEndDelimiter |> String.length) - 1

        let text = editor.Text

        let rec findRec startCount endCount at =
            if (text.Length <= at || at < 0) then None else

            let next = op at 1
            let st = try text.[at..(at+blockStartShift)] with | _ -> ""
            let en = try text.[at..(at+blockEndShift)] with | _ -> ""

            match st, en with
            | _, e when e = blockEndDelimiter && startCount < (endCount+1) -> Some at
            | _, e when e = blockEndDelimiter -> findRec startCount (endCount+1) next
            | s, _ when s = blockStartDelimiter -> findRec (startCount+1) endCount next
            | _,_ -> findRec startCount endCount next

        findRec 0 0 pos

    let rec findUnmatchedBlockStartDelimiter(editor:TextEditor) pos blockStartDelimiter blockEndDelimiter =
        findUnmatchedBlockDelimiter editor pos blockEndDelimiter blockStartDelimiter (-)

    let findUnmatchedBlockEndDelimiter(editor:TextEditor) pos blockStartDelimiter blockEndDelimiter =
        findUnmatchedBlockDelimiter editor pos blockStartDelimiter blockEndDelimiter (+)

    let isWordChar c = Char.IsLetterOrDigit c || c = '-' || c = '_'
    let isWORDChar c = not (Char.IsWhiteSpace c)
    let isNonBlankButNotWordChar c = isWORDChar c && not (isWordChar c)
    let isEOLChar c = c = '\r' || c = '\n'
    let isSpaceOrTab c = c = ' ' || c = '\t'

    let (|WhiteSpace|_|) c =
        if Char.IsWhiteSpace c then Some WhiteSpace else None

    let (|IsWordChar|_|) c =
        if isWordChar c then Some IsWordChar else None

    let (|EOLChar|_|) c =
        if isEOLChar c then Some EOLChar else None

    let findWordForwards (editor:TextEditor) commandType fWordChar =
        let findFromNonLetterChar index =
            match editor.[index], commandType with
            | EOLChar, Delete -> Some index
            | WhiteSpace, Move
            | WhiteSpace, Delete ->
                seq { index+1 .. editor.Length-1 }
                |> Seq.tryFind(fun index -> not (isSpaceOrTab editor.[index]))
                |> Option.bind(fun newIndex ->

                    let findFirstWordOnLine startOffset =
                        match editor.[startOffset] with
                        | WhiteSpace ->
                            seq { startOffset .. editor.Length-1 }
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
            seq { editor.CaretOffset+1 .. editor.Length-1 }
            |> Seq.tryFind(fun index -> index = editor.Length || not (fWordChar editor.[index]))
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
            |> Seq.tryFind(fun index -> index = editor.Length+1 || not (fWordChar editor.[index]))
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
        seq { editor.CaretOffset .. editor.Length - 2 }
        |> Seq.tryFind(fun index -> not (fWordChar editor.[index+1]))
        |> Option.defaultValue (editor.Length-1)

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
            | None -> editor.Length
        else
            currentWordEnd

    let findCurrentWordStart (editor:TextEditor) fWordChar =
        seq { editor.CaretOffset .. -1 .. 1 }
        |> Seq.tryFind(fun index -> not (fWordChar editor.[index-1]))
        |> Option.defaultValue 0

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
            | Some offset when offset + 1 < editor.Length ->
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

    /// Get the range of the trailing or leading whitespace
    /// around a word when aw or aW is used
    let getAroundWordRange (editor:TextEditor) wordStart wordEnd =
        let hasLeadingWhiteSpace =
            wordStart > 1 && Char.IsWhiteSpace editor.[wordStart-1]

        let hasTrailingWhiteSpace =
            wordEnd < editor.Length && Char.IsWhiteSpace editor.[wordEnd]

        let line = editor.GetLine editor.CaretLine
        match hasTrailingWhiteSpace, hasLeadingWhiteSpace with
        | true, _ ->
            let finish =
                seq { wordEnd .. line.EndOffset - 2 }
                |> Seq.tryFind(fun index -> not (Char.IsWhiteSpace editor.[index+1]))
                |> Option.defaultValue (line.EndOffset-1)
            wordStart, finish + 1
        | false, true ->
            let start =
                seq { wordStart .. -1 .. line.Offset }
                |> Seq.tryFind(fun index -> not (Char.IsWhiteSpace editor.[index-1]))
                |> Option.defaultValue line.Offset
            start, wordEnd
        | _ -> wordStart, wordEnd

    let getWordRange (editor:TextEditor) fWordChar =
        let wordStart = findCurrentWordStart editor fWordChar
        let wordEnd = findCurrentWordEnd editor fWordChar
        wordStart, wordEnd + 1

    let getWhitespaceRange (editor:TextEditor) fWordChar =
        let prevWordEnd = findWordBackwards editor Move fWordChar |> Option.defaultValue editor.CaretOffset
        let nextWordEnd = findWordEnd editor fWordChar
        prevWordEnd + 1, nextWordEnd + 1

    let rec findEnclosingTag(editor:TextEditor) pos =
        let search = seq { pos .. -1 .. 0 } |> Seq.tryFind(fun index -> editor.[index] = '<')
        match search with
        | Some startTagStart -> 
            let m = Regex.Match(editor.GetTextBetween(startTagStart, editor.Length), "<([\w|:|\.]+).*?>", RegexOptions.Singleline)
            if m.Success then 
                let tagName = m.Groups.[1].Value;
                let endTag = "</" + tagName + ">"
                let startTagEnd = startTagStart + m.Length - 1
                match findUnmatchedBlockEndDelimiter editor startTagEnd ("<" + tagName) endTag with 
                | Some endTagStart -> Some (startTagStart, startTagEnd, endTagStart, endTagStart + endTag.Length)
                | None -> findEnclosingTag editor (startTagStart - 1)
            else 
                None
        | None -> None

    let rec getRange (config:Config) (vimState:VimState) (editor:TextEditor) (command:VimAction) =
        let line = editor.GetLine editor.CaretLine
        let noOp = (editor.CaretOffset, editor.CaretOffset)
        match command.textObject with
        | Right behaviour ->
            let line = editor.GetLine editor.CaretLine
            let endOffset =
                match behaviour with
                | StopAtEndOfLine when editor.CaretColumn >= line.Length -> editor.CaretOffset
                | MoveToNextLineAtEnd when editor.[editor.CaretOffset+1] = '\r' -> editor.CaretOffset + 3
                | MoveToNextLineAtEnd when editor.[editor.CaretOffset+1] = '\n' -> editor.CaretOffset + 2
                | IncludeDelimiter ->
                    let line = editor.GetLine editor.CaretLine
                    if line.Length > 0 && editor.CaretColumn <= line.LengthIncludingDelimiter then
                        editor.CaretOffset + 1
                    else
                        editor.CaretOffset
                | _ -> editor.CaretOffset + 1
            editor.CaretOffset, endOffset
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
        | Character repeat ->
            let line = editor.GetLine editor.CaretLine
            let endOffset = min line.EndOffset (editor.CaretOffset + repeat)
            editor.CaretOffset, endOffset
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
                match config.keyboardLayout, vimState.keys with
                | Qwerty, Key ';' :: _ when c = editor.[editor.CaretOffset-1].ToString() ->
                    editor.CaretOffset-1
                | Colemak, Key 'o' :: _ when c = editor.[editor.CaretOffset-1].ToString() ->
                    editor.CaretOffset-1
                | Dvorak, Key 's' :: _ when c = editor.[editor.CaretOffset-1].ToString() ->
                    editor.CaretOffset-1
                | _, _ -> editor.CaretOffset
            match findStringCharBackwardsOnLine editor line startOffset c with
            | Some index -> editor.CaretOffset, index+1
            | None -> editor.CaretOffset, editor.CaretOffset
        | ToCharInclusive c ->
            match findCharForwardsOnLine editor line editor.CaretOffset c with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, editor.CaretOffset
        | ToCharExclusive c ->
            let startOffset =
                match config.keyboardLayout, vimState.keys with
                | Qwerty, Key ';' :: _ when c = editor.[editor.CaretOffset+1].ToString() ->
                    editor.CaretOffset+1
                | Colemak, Key 'o' :: _ when c = editor.[editor.CaretOffset+1].ToString() ->
                    editor.CaretOffset+1
                | Dvorak, Key 's' :: _ when c = editor.[editor.CaretOffset+1].ToString() ->
                    editor.CaretOffset+1
                | _, _ -> editor.CaretOffset
            match findCharForwardsOnLine editor line startOffset c with
            | Some index -> editor.CaretOffset, index-1
            | None -> editor.CaretOffset, editor.CaretOffset
        | InnerBlock (startChar, endChar) ->
            let opening = findUnmatchedBlockStartDelimiter editor editor.CaretOffset startChar endChar
            let closing = findUnmatchedBlockEndDelimiter editor editor.CaretOffset startChar endChar
            match opening, closing with
            | Some start, Some finish -> start+1, finish
            | _, _ -> editor.CaretOffset, editor.CaretOffset
        | ABlock (startChar, endChar) ->
            let opening = findUnmatchedBlockStartDelimiter editor editor.CaretOffset startChar endChar
            let closing = findUnmatchedBlockEndDelimiter editor editor.CaretOffset startChar endChar
            match opening, closing with
            | Some start, Some finish when finish < editor.Length -> start, finish+1
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
            | None -> editor.CaretOffset, editor.Length
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
            | None -> editor.CaretOffset, 0
        | Jump ParagraphForwards ->
            match paragraphForwards editor with
            | Some index -> editor.CaretOffset, index
            | None -> editor.CaretOffset, editor.CaretOffset
        | InnerWord ->
            let getWordCharFunc = function
                | IsWordChar -> isWordChar
                | _ -> isNonBlankButNotWordChar

            match editor.[editor.CaretOffset] with
            | WhiteSpace ->
                let matchFunc c = Char.IsWhiteSpace c && (not (isEOLChar c))
                getWordRange editor matchFunc
            | _ ->
                let fWordChar = getWordCharFunc editor.[editor.CaretOffset]
                getWordRange editor fWordChar
        | InnerWORD ->
            match editor.[editor.CaretOffset] with
            | WhiteSpace ->
                let matchFunc c = Char.IsWhiteSpace c && (not (isEOLChar c))
                getWordRange editor matchFunc
            | _ ->
                getWordRange editor isWORDChar
        | AWord ->
            let getWordCharFunc = function
                | IsWordChar -> isWordChar
                | _ -> isNonBlankButNotWordChar

            match editor.[editor.CaretOffset] with
            | WhiteSpace -> getWhitespaceRange editor isWordChar
            | _ ->
                let fWordChar = getWordCharFunc editor.[editor.CaretOffset]
                let wordStart, wordEnd = getWordRange editor fWordChar
                getAroundWordRange editor wordStart wordEnd
        | AWORD ->
            match editor.[editor.CaretOffset] with
            | WhiteSpace -> getWhitespaceRange editor isWORDChar
            | _ ->
                let wordStart, wordEnd = getWordRange editor isWORDChar
                getAroundWordRange editor wordStart wordEnd
        | ForwardToEndOfWord ->
            let isWordCharAtOffset offset = isWordChar (editor.[offset])

            match command.commandType, isWordCharAtOffset editor.CaretOffset, Char.IsWhiteSpace (editor.[editor.CaretOffset+1]) with
            | Change, true, true
            | Delete, true, true ->
                 editor.CaretOffset, editor.CaretOffset
            | _ -> editor.CaretOffset, findWordEnd editor isWordChar
        | ForwardToEndOfWORD -> editor.CaretOffset, findWordEnd editor isWORDChar
        | ATag -> 
            match findEnclosingTag editor editor.CaretOffset with
            | Some (startTagStart, _, _, endTagEnd) -> startTagStart, endTagEnd
            | None -> noOp
        | InnerTag -> 
            match findEnclosingTag editor editor.CaretOffset with
            | Some (_, startTagEnd, endTagStart, _) -> startTagEnd + 1, endTagStart
            | None -> noOp
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
            min lead anchor, max lead anchor
        | SelectionStart -> editor.CaretOffset, vimState.visualStartOffset
        | MatchingBrace ->
            match findNextBraceForwardsOnLine editor line with
            | Some offset ->
                let startOffset = editor.CaretOffset
                editor.CaretOffset <- offset
                EditActions.GotoMatchingBrace editor
                startOffset, editor.CaretOffset
            | _ -> editor.CaretOffset, editor.CaretOffset
        | PrevUnmatchedBrace -> 
            match findUnmatchedBlockStartDelimiter editor editor.CaretOffset "{" "}" with
            | Some jumpPos -> editor.CaretOffset, jumpPos
            | None -> noOp
        | NextUnmatchedBrace -> 
            match findUnmatchedBlockEndDelimiter editor editor.CaretOffset "{" "}" with
            | Some jumpPos -> editor.CaretOffset, jumpPos
            | None -> noOp
        | PrevUnmatchedParen -> 
            match findUnmatchedBlockStartDelimiter editor editor.CaretOffset "(" ")" with
            | Some jumpPos -> editor.CaretOffset, jumpPos
            | None -> noOp
        | NextUnmatchedParen -> 
            match findUnmatchedBlockEndDelimiter editor editor.CaretOffset "(" ")" with
            | Some jumpPos -> editor.CaretOffset, jumpPos
            | None -> noOp
        | Jump (ToMark (c, jumpType)) ->
            match markDict.TryGetValue c with
            | true, mark ->
                let offset =
                    match jumpType with
                    | MarkerJumpType.Offset -> mark.Offset
                    | MarkerJumpType.StartOfLine ->
                        let line = editor.GetLineByOffset mark.Offset
                        line.Offset + editor.GetLineIndent(line).Length

                if editor.FileName.FullPath.ToString() = mark.FileName then
                    editor.CaretOffset, offset
                else
                    let document = IdeApp.Workbench.GetDocument(mark.FileName)
                    let fileInfo = new MonoDevelop.Ide.Gui.FileOpenInformation (document.FileName, document.Project)
                    IdeApp.Workbench.OpenDocument(fileInfo) |> ignore
                    editor.CaretOffset, offset
            | _ -> editor.CaretOffset, editor.CaretOffset
        | Offset offset -> editor.CaretOffset, offset
        | Range (startOffset, endOffset) -> startOffset, endOffset
        | Jump (ToSearch search) ->
            let startOffset =
                match config.keyboardLayout, vimState.keys with
                | Qwerty, [Key 'n'] 
                | Qwerty, [Key 'N'] -> editor.CaretOffset + 1
                | Colemak, [Key 'k'] 
                | Colemak, [Key 'K'] -> editor.CaretOffset + 1
                | Dvorak, [Key 'b'] 
                | Dvorak, [Key 'B'] -> editor.CaretOffset + 1
                | _ -> editor.CaretOffset
            let offset = findNextSearchOffset editor search startOffset |> Option.defaultValue editor.CaretOffset
            editor.CaretOffset, offset
        | Jump (ToSearchBackwards search) ->
            let offset = findNextSearchOffsetBackwards editor search editor.CaretOffset |> Option.defaultValue editor.CaretOffset
            editor.CaretOffset, offset
        | Jump SearchAgain ->
            match vimState.lastSearch with
            | Some search -> getRange config vimState editor { command with textObject = search }
            | None -> editor.CaretOffset, editor.CaretOffset
        | Jump SearchAgainBackwards ->
            match vimState.lastSearch with
            | Some search ->
                let reverseSearch =
                    match search with
                    | Jump (ToSearch s) -> Jump (ToSearchBackwards s)
                    | Jump (ToSearchBackwards s) -> Jump (ToSearch s)
                    | _ -> failwith "Invalid search"
                getRange config vimState editor { command with textObject = reverseSearch }
            | None -> editor.CaretOffset, editor.CaretOffset
        | _ -> editor.CaretOffset, editor.CaretOffset

module Vim =
    LoggingService.LogInfo ("XSVim " + version)

    let registers = Dictionary<Register, XSVim.Selection>()
    let editorStates = Dictionary<FilePath, VimState>()

    registers.[EmptyRegister] <- { linewise=false; content="" }

    let macros = Dictionary<char, VimAction list>()

    let (|VisualModes|_|) = function
        | VisualMode | VisualLineMode | VisualBlockMode -> Some VisualModes
        | _ -> None

    let (|NotInsertMode|_|) = function
        | InsertMode | ReplaceMode | ExMode _ -> None
        | _ -> Some NotInsertMode

    let setSelection vimState (editor:TextEditor) (command:VimAction) (start:int) finish =
        match vimState.mode, command.commandType with
        | VisualMode, Move | VisualMode, SwitchMode _ ->
            let start, finish =
                if finish < vimState.visualStartOffset then
                    finish, vimState.visualStartOffset + 1
                else
                    vimState.visualStartOffset, finish + if command.textObject = EndOfLine then 0 else 1

            editor.SetSelection(start, min finish editor.Length)
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
        | _ -> editor.SetSelection(start, min finish editor.Length)

    let (|MoveUpOrDown|_|) = function
        | { commandType=Move; textObject=Up }
        | { commandType=Move; textObject=Down } -> Some MoveUpOrDown
        | _ -> None

    let (|LineWise|_|) = function
        | Up | Down
        | WholeLine
        | WholeLineIncludingDelimiter
        | Jump (ToMark (_, MarkerJumpType.StartOfLine))
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

    let getCaretMode (editor:TextEditor) =
        let caret = editor.Carets.[0]
        let caretMode:bool = caret?IsInInsertMode
        match caretMode with
        | true -> Insert
        | false -> Block

    let setCaretMode (editor:TextEditor) caretMode =
        let currentMode = getCaretMode editor
        match currentMode, caretMode with
        | Block, Insert -> EditActions.SwitchCaretMode editor
        | Insert, Block -> EditActions.SwitchCaretMode editor
        | _ -> ()

    let setAutoCompleteOnKeystroke value =
        if SettingsPanel.AutoCompleteInNormalModeIsDisabled() then
            IdeApp.Preferences.EnableAutoCodeCompletion.Set value |> ignore

    let switchToInsertMode (editor:TextEditor) state isInitial =
        let group =
            if isInitial
                then editor.OpenUndoGroup() |> Some
            else
                state.undoGroup

        setAutoCompleteOnKeystroke true
        setCaretMode editor Insert
        { state with mode = InsertMode; statusMessage = "-- INSERT --" |> Some; keys = []; undoGroup = group }

    let switchToNormalMode (editor:TextEditor) vimState =
        let lastSelection =
            match vimState.mode with
            | VisualModes ->
                Some { start = vimState.visualStartOffset; finish = editor.CaretOffset; mode = vimState.mode }
            | _ -> vimState.lastSelection
        editor.ClearSelection()
        setAutoCompleteOnKeystroke false
        setCaretMode editor Block
        // stupid hack to prevent intellisense in normal mode
        // https://github.com/mono/monodevelop/blob/fdbfbe89529bd9076e1906e7b70fdb51a9ae6b99/main/src/core/MonoDevelop.Ide/MonoDevelop.Ide.Editor.Extension/CompletionTextEditorExtension.cs#L153
        if editor.SelectionMode = SelectionMode.Normal then EditActions.ToggleBlockSelectionMode editor
        vimState.undoGroup |> Option.iter(fun d -> d.Dispose())
        if vimState.mode = InsertMode && editor.CaretColumn > 1 then
            EditActions.MoveCaretLeft editor
        { vimState with mode = NormalMode; lastSelection = lastSelection; undoGroup = None; statusMessage = None }

    let processVimKey (editor:TextEditor) =
        function
        | Key k -> editor.InsertAtCaret (string k)
        | VimKey.Delete -> EditActions.Delete editor
        | VimKey.Backspace -> EditActions.Backspace editor
        | VimKey.Left -> EditActions.MoveCaretLeft editor
        | VimKey.Right -> EditActions.MoveCaretRight editor
        | VimKey.Up -> EditActions.MoveCaretUp editor
        | VimKey.Down -> EditActions.MoveCaretDown editor
        | Ret -> EditActions.InsertNewLine editor
        | Super _
        | Esc
        | EscapeKey _
        | Control _ -> ()

    let runCommand config vimState editor command =
        let delete state start finish =
            let finish =
                match command.textObject with
                | ForwardToEndOfWord
                | ForwardToEndOfWORD
                | EndOfLine
                | MatchingBrace
                | PrevUnmatchedBrace
                | NextUnmatchedBrace
                | PrevUnmatchedParen
                | NextUnmatchedParen
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

        let rec processCommands config count vimState command isInitial =
            let blockInsert fColumnSelect =
                let selectionStartLocation = editor.OffsetToLocation vimState.visualStartOffset
                let topLine = min selectionStartLocation.Line editor.CaretLine
                let bottomLine = max selectionStartLocation.Line editor.CaretLine
                editor.CaretColumn <- fColumnSelect editor.CaretColumn selectionStartLocation.Column
                editor.SetSelection(DocumentLocation (topLine, editor.CaretColumn), DocumentLocation (bottomLine, editor.CaretColumn))
                if editor.SelectionMode = SelectionMode.Normal then EditActions.ToggleBlockSelectionMode editor
                switchToInsertMode editor vimState isInitial

            let start, finish =
                if editor.Length > 0 then
                    VimHelpers.getRange config vimState editor command
                else
                    // editor can have zero length when a tab containing it has just been closed
                    0, 0

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
                    let linewise = isLineWise vimState command
                    let start, finish =
                        match linewise with
                        | true ->

                            let min = min start finish
                            let maxOffset = max start finish
                            let line = editor.GetLineByOffset min
                            let finish = editor.GetLineByOffset(maxOffset).EndOffsetIncludingDelimiter
                            if eofOnLine line && line.LineNumber <> 1 then
                                let delimiter = inferDelimiter editor
                                line.Offset-delimiter.Length, finish
                            else
                                line.Offset, finish
                        | false -> start, finish

                    let newState = delete vimState start finish
                    let offsetBeforeDelimiter =
                        match linewise with
                        | true ->
                            let line = editor.GetLineByOffset(editor.CaretOffset)
                            let line =
                                if editor.CaretOffset = editor.Length && editor.Length > 0 && editor.[editor.Length-1] = '\n' && line.PreviousLine <> null then
                                    line.PreviousLine
                                else
                                    line
                            line.Offset + editor.GetLineIndent(line.LineNumber).Length
                        | false when editor.CaretOffset < editor.Length && editor.CaretOffset > 0 ->
                            let charAtCaret = editor.[editor.CaretOffset]
                            let previous = editor.[editor.CaretOffset - 1]

                            if isEOLChar charAtCaret && not (isEOLChar previous) then
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
                | EqualIndent ->
                    // Always work top to bottom
                    let start, finish = min start finish, max start finish
                    let finish =
                        if editor.[finish-1] = '\n' then
                            finish - 1
                        else
                            finish
                    let startLine = editor.GetLineByOffset(start)
                    let endLine = editor.GetLineByOffset(finish).LineNumber

                    for line in [startLine.LineNumber..endLine] do
                        let currentIndent = editor.GetLineIndent (line)
                        let newIndent = editor.IndentationTracker.GetIndentationString(line-1)
                        editor.ReplaceText(editor.GetLine(line).Offset, currentIndent.Length, newIndent)

                    editor.CaretOffset <-
                        let startLine = editor.GetLineByOffset(start)
                        startLine.Offset + editor.GetLineIndent(startLine).Length
                    vimState
                | Substitute ->
                    let newState = delete vimState start finish
                    switchToInsertMode editor newState isInitial
                | ToggleCase ->
                    toggleCase vimState start finish
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
                    processCommands config 1 vimState (runOnce (SwitchMode NormalMode) Nothing) false
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
                            | _ -> editor.InsertText(editor.Length, delimiter)
                            editor.CaretOffset <- editor.Length
                            EditActions.ClipboardPaste editor
                            if eofOnLine line then
                                match registers.[EmptyRegister].content with
                                | EndsWithDelimiter clipboardDelimiter ->
                                    editor.RemoveText(editor.Length-clipboardDelimiter.Length, clipboardDelimiter.Length)
                                | _ -> ()
                            editor.CaretOffset <- line.Offset
                            EditActions.MoveCaretDown editor
                            EditActions.MoveCaretToLineStart editor
                        else
                            let line = editor.GetLine(editor.CaretLine)
                            editor.CaretOffset <- line.EndOffsetIncludingDelimiter
                            EditActions.ClipboardPaste editor
                            editor.CaretOffset <- line.Offset
                            EditActions.MoveCaretDown editor
                            EditActions.MoveCaretToLineStart editor
                    else
                        EditActions.MoveCaretRight editor
                        EditActions.ClipboardPaste editor
                        EditActions.MoveCaretLeft editor
                    vimState
                | Put OverSelection ->
                    EditActions.ClipboardPaste editor
                    { vimState with mode = NormalMode }
                | SelectionOtherEnd ->
                    let offset = editor.CaretOffset
                    editor.CaretOffset <- vimState.visualStartOffset
                    let start, finish =
                        if offset > vimState.visualStartOffset then
                            vimState.visualStartOffset, offset + 1
                        else
                            offset, vimState.visualStartOffset + 1
                    editor.SetSelection(start, finish)
                    { vimState with visualStartOffset = offset }
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
                    if editor.CaretOffset < editor.Length  && not (isEOLChar editor.[editor.CaretOffset]) then
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
                        let state = switchToNormalMode editor vimState
                        if vimState.mode = InsertMode then
                            processCommands config 1 state (runOnce (SetMark ".") Nothing) false
                        else
                            state
                    | VisualMode | VisualLineMode | VisualBlockMode ->
                        setCaretMode editor Block
                        let start, finish = VimHelpers.getRange config vimState editor command
                        let statusMessage =
                            match mode with
                            | VisualMode -> Some "-- VISUAL --"
                            | VisualLineMode -> Some "-- VISUAL LINE --"
                            | VisualBlockMode -> Some "-- VISUAL BLOCK --"
                            | _ -> None
                        let newState = { vimState with mode = mode; visualStartOffset = editor.CaretOffset; statusMessage = statusMessage }
                        setAutoCompleteOnKeystroke false
                        setSelection newState editor command start finish
                        match mode, editor.SelectionMode with
                        | VisualBlockMode, SelectionMode.Normal -> EditActions.ToggleBlockSelectionMode editor
                        | _, SelectionMode.Block -> EditActions.ToggleBlockSelectionMode editor
                        | _ -> ()
                        newState
                    | InsertMode ->
                        switchToInsertMode editor vimState isInitial
                    | ReplaceMode ->
                        let undoGroup =
                            if isInitial
                                then editor.OpenUndoGroup() |> Some
                            else
                                vimState.undoGroup

                        { vimState with mode = ReplaceMode; statusMessage = "-- REPLACE --"|> Some; undoGroup = undoGroup }
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
                        processCommands config 1 vimState (runOnce Move WordForwards) isInitial
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
                        processVimKey editor c
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
                            processCommands config (getCount only.repeat) state only false
                        | h :: t ->
                            let newState = processCommands config (getCount h.repeat) state h false
                            runMacro newState t
                        | [] -> state
                    runMacro vimState macros.[c]
                | NextTab ->
                    Window.nextTab()
                    vimState
                | PreviousTab ->
                    Window.previousTab()
                    vimState
                | Func f ->
                    f()
                    vimState
                | EditorFunc f ->
                    f editor
                    vimState
                | GotoPad padId ->
                    Window.gotoPad padId
                    vimState
                | ChangeState s -> s
                | DelayedFunc (f, ms) ->
                    let token = new CancellationTokenSource()
                    let work =
                        async {
                            do! Async.Sleep ms
                            if (not token.IsCancellationRequested) then
                                Runtime.RunInMainThread(fun _ -> f editor) |> ignore
                        }
                    Async.Start(work, token.Token)
                    { vimState with insertModeCancellationTokenSource = Some token }
                | CancelFunc ->
                    vimState.insertModeCancellationTokenSource
                    |> Option.iter(fun token -> token.Cancel())
                    { vimState with insertModeCancellationTokenSource = None }
                | _ -> vimState

            match count with
            | 1 -> newState
            | _ -> processCommands config (count-1) newState command false
        let count = command.repeat |> Option.defaultValue 1

        processCommands config count vimState command true

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

    let (|BlockDelimiter|_|) layout character =
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
        let mappedChar = remap layout character
        if pairs.ContainsKey mappedChar then
            Some pairs.[mappedChar]
        else
            None

    let (|QuoteDelimiter|_|) layout character =
        let mappedChar = remap layout character
        if Array.contains mappedChar [| "\""; "'"; "`"|] then
            Some mappedChar
        else
            None

    let (|Movement|_|) layout keys =
        let remappedKeys = keys |> List.map (fun k -> remap layout k)
        match remappedKeys with
        | ["<left>"]
        | ["h"] -> Some Left
        | ["<down>"]
        | ["j"] -> Some Down
        | ["<up>"]
        | ["k"] -> Some Up
        | ["<right>"]
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
        | ["["; "{"] -> Some PrevUnmatchedBrace
        | ["]"; "}"] -> Some NextUnmatchedBrace
        | ["["; "("] -> Some PrevUnmatchedParen
        | ["]"; ")"] -> Some NextUnmatchedParen
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
        | ["'"; c] -> Some (Jump (ToMark (c, MarkerJumpType.StartOfLine)))
        | ["`"; c] -> Some (Jump (ToMark (c, MarkerJumpType.Offset)))
        | _ -> None

    let unfinishedMovements = [ "g"; "["; "]"; "@"; "m"; "`"; "'" ] |> set

    let (|UnfinishedMovement|_|) layout character =
        let remappedCharacter = remap layout character
        if unfinishedMovements.Contains remappedCharacter then
            Some UnfinishedMovement
        else
            None

    let (|IndentChar|_|) layout key =
        match remap layout key with
        | ">" -> Some Indent
        | "<" -> Some UnIndent
        | "=" -> Some EqualIndent
        | _ -> None

    let (|FindChar|_|) layout key =
        match remap layout key with
        | "f" -> Some ToCharInclusive
        | "F" -> Some ToCharInclusiveBackwards
        | "t" -> Some ToCharExclusive
        | "T" -> Some ToCharExclusiveBackwards
        | _ -> None

    let (|SearchChar|_|) layout key =
        match remap layout key with
        | "/" -> Some (SearchChar '/')
        | "?" -> Some (SearchChar '?')
        | _ -> None

    let (|Action|_|) layout key =
        match remap layout key with
        | "d" -> Some Delete
        | "c" -> Some Change
        | "v" -> Some Visual
        | "y" -> Some (Yank EmptyRegister)
        | ">" -> Some Indent
        | "<" -> Some UnIndent
        | _ -> None

    let (|ModeChange|_|) layout key =
        match remap layout key with
        | "i" -> Some InsertMode
        | "v" -> Some VisualMode
        | "<C-v>" -> Some VisualBlockMode
        | "<C-q>" -> Some VisualBlockMode
        | "V" -> Some VisualLineMode
        | "R" -> Some ReplaceMode
        | _ -> None

    let (|Escape|_|) = function
        | "<esc>" | "<C-c>" | "<C-[>" -> Some Escape
        | _ -> None

    let (|RemappedMatches|_|) layout matchList unmappedList = 
        let mappedList = unmappedList |> List.map (fun x -> remap layout x)
        if mappedList = matchList then 
          Some matchList
        else 
          None

    let (|RemappedMatchesChar|_|) layout matchChar unmappedChar = //
        let mappedChar = remap layout unmappedChar
        if mappedChar = matchChar then 
          Some matchChar
        else 
          None
                
    let getInsertModeEscapeCombo config =
        match config.insertModeEscapeKey with
        | Some combo ->
            combo.insertModeEscapeKey1, combo.insertModeEscapeKey2, combo.insertModeEscapeTimeout
        | None -> "", "", 0

    let parseKeys (state:VimState) (config: Config) =
        let layout = config.keyboardLayout;
        let keyList = state.keys |> List.map string
        let numericArgument, keyList =
            match keyList, state.mode with
            | "r" :: _, _
            | [ _ ], ReplaceMode
            | FindChar layout _ :: _, _ -> None, keyList
            // 2dw -> 2, dw
            | OneToNine d1 :: Digit d2 :: Digit d3 :: Digit d4 :: t, _ ->
                Some (d1 * 1000 + d2 * 100 + d3 * 10 + d4), t
            | OneToNine d1 :: Digit d2 :: Digit d3 :: t, _ ->
                Some (d1 * 100 + d2 * 10 + d3), t
            | OneToNine d1 :: Digit d2 :: t, _ ->
                Some (d1 * 10 + d2), t
            | OneToNine d :: t, _ -> Some (d), t
            // d2w -> 2, dw
            | c :: OneToNine d1 :: Digit d2 :: Digit d3 :: Digit d4 :: t, _ ->
                Some (d1 * 1000 + d2 * 100 + d3 * 10 + d4), c::t
            | c :: OneToNine d1 :: Digit d2 :: Digit d3 :: t, _ ->
                Some (d1 * 100 + d2 * 10 + d3), c::t
            | c :: OneToNine d1 :: Digit d2 :: t, _ ->
                Some (d1 * 10 + d2), c::t
            | c :: OneToNine d :: t, _ ->
                Some d, c::t
            | _ -> None, keyList

        let run = getCommand numericArgument

        let runInVisualMode actions = [ yield switchMode VisualMode; yield! actions; yield switchMode NormalMode ]
        let runInVisualLineMode actions = [ yield switchMode VisualLineMode; yield! actions; yield switchMode NormalMode ]

        LoggingService.LogDebug (sprintf "%A %A" state.mode keyList)
        let newState =
            match keyList with
            | [ FindChar layout m; c ] -> { state with findCharCommand = run Move ( m c ) |> Some }
            | _ -> state

        let insertModeEscapeFirstChar, insertModeEscapeSecondChar, insertModeTimeout =
            getInsertModeEscapeCombo config

        let action =
            match state.mode, keyList with
            | VisualBlockMode, [ Escape ] -> [ switchMode NormalMode; run Move SelectionStart ]
            | NormalMode, [ Escape ] -> [ yield! resetKeys; yield dispatch "MonoDevelop.Ide.Commands.ViewCommands.FocusCurrentDocument" ]
            | _, [ Escape ] -> [ switchMode NormalMode ]
            | InsertMode, [ c ] when c = insertModeEscapeFirstChar ->
                delayedFunc (fun editor ->
                                 editor.InsertAtCaret insertModeEscapeFirstChar
                                 let oldState = editorStates.[editor.FileName]
                                 editorStates.[editor.FileName] <- { oldState with keys = [] }  ) insertModeTimeout :: wait
            | InsertMode, [ c1; c2 ] when c1 = insertModeEscapeFirstChar && c2 = insertModeEscapeSecondChar ->
                [ run CancelFunc Nothing; switchMode NormalMode ]
            | InsertMode, [ c; _ ] when c = insertModeEscapeFirstChar ->
                [ run CancelFunc Nothing
                  run (ChangeState { state with keys = [] }) Nothing
                  typeChar (Key (char insertModeEscapeFirstChar)) ]
            | NotInsertMode, RemappedMatches layout [ "G" ] _ ->
                match numericArgument with
                | Some lineNumber -> [ runOnce Move (Jump (StartOfLineNumber lineNumber)) ]
                | None -> [ runOnce Move (Jump LastLine) ]
            | NormalMode, [ IndentChar layout _ ] -> wait
            | NormalMode, [ IndentChar layout _ ; RemappedMatchesChar layout "g" _ ] -> wait
            | NormalMode, [ IndentChar layout indent; RemappedMatchesChar layout "G" _ ] ->
                match numericArgument with
                | Some lineNumber -> [ runOnce Indent (Jump (StartOfLineNumber lineNumber)) ]
                | None -> [ runOnce indent (Jump LastLine) ]
            | NormalMode, [ IndentChar layout indent; RemappedMatchesChar layout "g" _; RemappedMatchesChar layout "g" _ ] ->
                let lineNumber = match numericArgument with Some n -> n | None -> 1
                [ runOnce indent (Jump (StartOfLineNumber lineNumber)) ]
            | NormalMode, RemappedMatches layout [ ">"; ">" ] _ -> [ run Indent WholeLine ]
            | NormalMode, RemappedMatches layout [ "<"; "<" ] _ -> [ run UnIndent WholeLine ]
            | NormalMode, RemappedMatches layout [ "="; "=" ] _ -> [ run EqualIndent WholeLine ]
            | NormalMode, RemappedMatches layout [ "V" ] _ ->
                match numericArgument with
                | Some lines -> [ switchMode VisualLineMode; getCommand (lines-1 |> Some) Move Down ]
                | None -> [ switchMode VisualLineMode ]
            | NormalMode, RemappedMatches layout [ "v" ] _ ->
                match numericArgument with
                | Some chars -> [ switchMode VisualMode; getCommand (chars-1 |> Some) Move (Right StopAtEndOfLine) ]
                | None -> [ switchMode VisualMode ]
            | NormalMode, RemappedMatches layout [ "d"; "G" ] _ -> [ runOnce DeleteWholeLines (Jump LastLine)]
            | NormalMode, RemappedMatches layout [ "d"; "j" ] _ ->
                let numberOfLines =
                    match numericArgument with
                    | Some lines -> lines
                    | None -> 1
                runInVisualLineMode [ getCommand (numberOfLines |> Some) Move Down; runOnce Delete SelectedText ]
            | NormalMode, RemappedMatches layout [ "d"; "k" ] _ ->
                let numberOfLines =
                    match numericArgument with
                    | Some lines -> lines
                    | None -> 1
                runInVisualLineMode [ getCommand (numberOfLines |> Some) Move Up; runOnce Delete SelectedText; ]
            | NotInsertMode, [ (Action layout _) ; UnfinishedMovement layout _] -> wait
            | NotInsertMode, [ UnfinishedMovement layout _ ] -> wait
            | NormalMode, RemappedMatches layout [ "d"; "g"; "g" ] _ -> [ runOnce DeleteWholeLines (Jump StartOfDocument)]
            | ReplaceMode, [ c ] -> [ run (ReplaceChar c) Nothing; run Move (Right IncludeDelimiter) ]
            | NotInsertMode, Movement layout m -> [ run Move m ]
            | NotInsertMode, [ FindChar layout m; c ] -> [ run Move (m c) ]
            | NormalMode, IndentChar layout indent :: Movement layout m ->
                match numericArgument with
                | None -> [ run indent m ]
                | Some lines ->
                    runInVisualMode [ getCommand (lines-1 |> Some) Move Down; runOnce indent SelectedText ]
            | NormalMode, Action layout action :: Movement layout m when numericArgument = None -> [ run action m ]
            | NormalMode, Action layout action :: Movement layout m ->
                match action, m with
                | Delete, _
                | Yank _, _ ->
                    match m with
                    | WordForwards -> runInVisualMode [ run Move ForwardToEndOfWord; runOnce Move (Right StopAtEndOfLine); runOnce action SelectedText; ]
                    | WORDForwards -> runInVisualMode [ run Move ForwardToEndOfWORD; runOnce Move (Right StopAtEndOfLine); runOnce action SelectedText; ]
                    | WordBackwards -> runInVisualMode [ runOnce Move Left; run Move ForwardToEndOfWord; runOnce action SelectedText; ]
                    | WORDBackwards -> runInVisualMode [ runOnce Move Left; run Move ForwardToEndOfWORD; runOnce action SelectedText; ]
                    | _ -> runInVisualMode [ run Move m; runOnce action SelectedText; ]
                | _ -> [ run action m ]
            | NormalMode, RemappedMatches layout [ "u" ] _ -> [ run Undo Nothing ]
            | NormalMode, [ "<C-r>" ] -> [ run Redo Nothing ]
            | NormalMode, RemappedMatches layout [ "d"; "d" ] _ ->
                match numericArgument with
                | None -> [ run Delete WholeLine ]
                | Some lines ->
                    [ switchMode VisualLineMode
                      getCommand (lines-1 |> Some) Move Down
                      runOnce Delete SelectedText
                      switchMode NormalMode
                      runOnce Move FirstNonWhitespace ]
            | NormalMode, RemappedMatches layout [ "c"; "c" ] _ -> [ run Change WholeLine ]
            | NormalMode, RemappedMatches layout ["\""] _ -> wait
            | NormalMode, [RemappedMatchesChar layout "\"" _; _ ] -> wait
            | NormalMode, [RemappedMatchesChar layout "\"" _; _; RemappedMatchesChar layout "y" _ ] -> wait
            | NormalMode, RemappedMatchesChar layout "\"" _ :: (RegisterMatch r) :: RemappedMatchesChar layout "y" _ :: (Movement layout m) -> [ run (Yank r) m]
            | NormalMode, RemappedMatches layout [ "y"; "y" ] _
            | NormalMode, RemappedMatches layout [ "Y" ] _ ->
                match numericArgument with
                | Some lines -> runInVisualLineMode [ getCommand (lines-1 |> Some) Move Down; runOnce (Yank EmptyRegister) SelectedText ]
                | None -> [ runOnce (Yank EmptyRegister) WholeLineIncludingDelimiter ]
            | NormalMode, RemappedMatches layout [ "C" ] _ -> [ run Change EndOfLine ]
            | NormalMode, RemappedMatches layout [ "D" ] _ -> [ run Delete EndOfLine ]
            | NormalMode, RemappedMatches layout [ "x" ] _ -> [ runOnce Delete (Character (numericArgument |> Option.defaultValue 1)) ]
            | NormalMode, RemappedMatches layout [ "X" ] _ -> [ run DeleteLeft Nothing ]
            | NormalMode, RemappedMatches layout [ "s" ] _ -> [ run Substitute CurrentLocation]
            | NormalMode, RemappedMatches layout [ "S" ] _ -> [ run Delete WholeLine; runOnce (InsertLine After) Nothing; switchMode InsertMode ]
            | NormalMode, RemappedMatches layout [ "p" ] _ -> [ run (Put After) Nothing ]
            | NormalMode, RemappedMatches layout [ "P" ] _ -> [ run (Put Before) Nothing ]
            | VisualModes, RemappedMatches layout [ "p" ] _ -> [ run (Put OverSelection) Nothing ]
            | VisualModes, RemappedMatches layout [ "P" ] _ -> [ run (Put OverSelection) Nothing ]
            | NormalMode, RemappedMatches layout [ "J" ] _ -> [ run JoinLines Nothing ]
            | NotInsertMode, RemappedMatches layout [ "*" ] _ -> [ run (Star After) Nothing ]
            | NotInsertMode, RemappedMatches layout [ "#" ] _ -> [ run (Star Before) Nothing ]
            | NotInsertMode, RemappedMatches layout [ "£" ] _ -> [ run (Star Before) Nothing ]
            | NotInsertMode, [ SearchChar layout c ] -> [ switchMode (ExMode (string c)); runOnce (SetSearchAction Move) Nothing ]
            | VisualModes, RemappedMatches layout [ ":" ] _ -> [ switchMode (ExMode ":'<,'>") ]
            | NotInsertMode, RemappedMatches layout [ ":" ] _ -> [ switchMode (ExMode ":") ]
            | NotInsertMode, [ Action layout action; SearchChar layout c ] -> [ switchMode (ExMode (string c)); runOnce (SetSearchAction action) Nothing ]
            | NormalMode, RemappedMatches layout [ "z"; "z" ] _ -> [ dispatch ViewCommands.CenterAndFocusCurrentDocument ]
            | NormalMode, RemappedMatches layout [ "z"; ] _ -> wait
            | NormalMode, [ "<C-y>" ] -> [ dispatch TextEditorCommands.ScrollLineUp ]
            | NormalMode, [ "<C-e>" ] -> [ dispatch TextEditorCommands.ScrollLineDown ]
            | NormalMode, [ "<C-o>" ] -> [ dispatch NavigationCommands.NavigateBack ]
            | NormalMode, [ "<C-i>" ] -> [ dispatch NavigationCommands.NavigateForward ]
            | NormalMode, RemappedMatches layout [ "r" ] _ -> wait
            | NormalMode, [ RemappedMatchesChar layout "r" _; "<ret>" ] -> [ run (ReplaceChar "\n" ) Nothing ]
            | NormalMode, [ RemappedMatchesChar layout "r" _; c ] -> [ run (ReplaceChar c) Nothing ]
            | NormalMode, [ RemappedMatchesChar layout "m" _; c ] -> [ run (SetMark c) Nothing ]
            | NotInsertMode, [ Action layout action; FindChar layout m; c ] -> [ run action (m c) ]
            | NotInsertMode, [ Action layout action; RemappedMatchesChar layout "i" _; BlockDelimiter layout c ] -> [ run action (InnerBlock c) ]
            | NotInsertMode, [ Action layout action; RemappedMatchesChar layout "a" _; BlockDelimiter layout c ] -> [ run action (ABlock c) ]
            | NotInsertMode, [ Action layout action; RemappedMatchesChar layout "i" _; QuoteDelimiter layout c ] -> [ run action (InnerQuotedBlock (char c)) ]
            | NotInsertMode, [ Action layout action; RemappedMatchesChar layout "a" _; QuoteDelimiter layout c ] -> [ run action (AQuotedBlock (char c)) ]
            | NotInsertMode, [ Action layout action; RemappedMatchesChar layout "i" _; RemappedMatchesChar layout "w" _ ] -> [ run action InnerWord ]
            | NotInsertMode, [ Action layout action; RemappedMatchesChar layout "a" _; RemappedMatchesChar layout "w" _ ] -> [ run action AWord ]
            | NotInsertMode, [ Action layout action; RemappedMatchesChar layout "i" _; RemappedMatchesChar layout "W" _ ] -> [ run action InnerWORD ]
            | NotInsertMode, [ Action layout action; RemappedMatchesChar layout "a" _; RemappedMatchesChar layout "W" _ ] -> [ run action AWORD ]
            | NotInsertMode, [ Action layout action; RemappedMatchesChar layout "a" _; RemappedMatchesChar layout "t" _ ] -> [ run action ATag ]
            | NotInsertMode, [ Action layout action; RemappedMatchesChar layout "i" _; RemappedMatchesChar layout "t" _ ] -> [ run action InnerTag ]
            | VisualMode, RemappedMatches layout [ "i"; "w" ] _ -> [ run Visual InnerWord ]
            | VisualMode, RemappedMatches layout [ "a"; "w" ] _ -> [ run Visual AWord ]
            | VisualMode, RemappedMatches layout [ "i"; "W" ] _ -> [ run Visual InnerWORD ]
            | VisualMode, RemappedMatches layout [ "a"; "W" ] _ -> [ run Visual AWORD ]
            | VisualMode, RemappedMatches layout [ "a"; "t" ] _ -> [ run Visual ATag ]
            | VisualMode, RemappedMatches layout [ "i"; "t" ] _ -> [ run Visual InnerTag ]
            | VisualMode, RemappedMatches layout [ "u"] _ -> [ dispatch EditCommands.LowercaseSelection ]
            | VisualMode, RemappedMatches layout [ "U"] _ -> [ dispatch EditCommands.UppercaseSelection ]
            | NormalMode, [ ModeChange layout mode ] -> [ switchMode mode ]
            | NormalMode, RemappedMatches layout [ "a" ] _ -> [ run Move (Right IncludeDelimiter); switchMode InsertMode ]
            | NormalMode, RemappedMatches layout [ "A" ] _ -> [ run Move EndOfLineIncludingDelimiter; switchMode InsertMode ]
            | NormalMode, RemappedMatches layout [ "O" ] _ -> [ run (InsertLine After) Nothing; switchMode InsertMode ]
            | NormalMode, RemappedMatches layout [ "o" ] _ -> [ run (InsertLine Before) Nothing; switchMode InsertMode ]
            | NormalMode, RemappedMatches layout [ "I" ] _ -> [ run Move FirstNonWhitespace; switchMode InsertMode ]
            | NormalMode, [ Action layout _ ] -> wait
            | NotInsertMode, [ Action layout _; RemappedMatchesChar layout "i" _ ] -> wait
            | NotInsertMode, [ Action layout _; "a" ] -> wait
            | VisualMode, RemappedMatches layout [ "i" ] _ | VisualMode, [ "a" ] -> wait
            | NotInsertMode, [ FindChar layout _; ] -> wait
            | NotInsertMode, [ Action layout _; FindChar layout _; ] -> wait | NotInsertMode, [ "<ret>" ] -> [ run Move Down; run Move FirstNonWhitespace ]
            | NotInsertMode, RemappedMatches layout [ "q" ] _ when state.macro.IsNone -> wait
            | NotInsertMode, [ RemappedMatchesChar layout "q" _; c ] -> [ run (MacroStart (char c)) Nothing ]
            | NotInsertMode, RemappedMatches layout [ "q" ] _ -> [ run MacroEnd Nothing ]
            | NotInsertMode, [ "@"; c ] -> [ run (ReplayMacro (char c)) Nothing ]
            | NotInsertMode, RemappedMatches layout [ "g"; "g" ] _ ->
                let lineNumber = match numericArgument with Some n -> n | None -> 1
                [ runOnce Move (Jump (StartOfLineNumber lineNumber)) ]
            | NotInsertMode, RemappedMatches layout [ "g"; "d" ] _ -> [ dispatch "MonoDevelop.Refactoring.RefactoryCommands.GotoDeclaration" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "u" ] _ -> [ dispatch "MonoDevelop.Refactoring.RefactoryCommands.FindReferences" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "b" ] _ -> [ dispatch "MonoDevelop.RefactoryCommands.NavigationCommands.FindBaseSymbols" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "t" ] _ -> [ func Window.nextTab ]
            | NotInsertMode, RemappedMatches layout [ "g"; "T" ] _ -> [ func Window.previousTab ]
            | NotInsertMode, RemappedMatches layout [ "z"; "z" ] _ -> [ dispatch TextEditorCommands.RecenterEditor ]
            | NotInsertMode, RemappedMatches layout [ "z"; "a" ] _ -> [ dispatch EditCommands.ToggleAllFoldings ]
            | NotInsertMode, RemappedMatches layout [ "z"; "o" ] _ -> [ dispatch EditCommands.ToggleFolding ]
            | NotInsertMode, RemappedMatches layout [ "z"; "c" ] _ -> [ dispatch EditCommands.ToggleFolding ]
            | NotInsertMode, RemappedMatches layout [ "g"; "h" ] _ -> [ dispatch TextEditorCommands.ShowQuickInfo ]
            | NotInsertMode, RemappedMatches layout [ "g"; "v" ] _ ->
                match state.lastSelection with
                | Some selection -> [ run Move (Offset selection.start)
                                      switchMode selection.mode
                                      run Move (Offset selection.finish) ]
                | None -> resetKeys
            | NotInsertMode, RemappedMatches layout [ "." ] _ -> state.lastAction @ [ switchMode NormalMode ]
            | NotInsertMode, RemappedMatches layout [ ";" ] _ -> match state.findCharCommand with Some command -> [ command ] | None -> []
            | NotInsertMode, RemappedMatches layout [ "," ] _ ->
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
            | VisualModes, Movement layout m -> [ run Move m ]
            | VisualBlockMode, RemappedMatches layout [ "I" ] _ -> [ run (BlockInsert Before) Nothing ]
            | VisualBlockMode, [ "A" ] -> [ run (BlockInsert After) Nothing ]
            | VisualModes, [ RemappedMatchesChar layout "i" _; BlockDelimiter layout c ] -> [ run Visual (InnerBlock c) ]
            | VisualModes, [ "a"; BlockDelimiter layout c ] -> [ run Visual (ABlock c) ]
            | VisualModes, [ RemappedMatchesChar layout "i" _; QuoteDelimiter layout c ] -> [ run Visual (InnerQuotedBlock (char c)) ]
            | VisualModes, [ "a"; QuoteDelimiter layout c ] -> [ run Visual (AQuotedBlock (char c)) ]
            | VisualModes, RemappedMatches layout [ "x" ] _ -> [ run Delete SelectedText; switchMode NormalMode ]
            | VisualModes, RemappedMatches layout [ "d" ] _ -> [ run Delete SelectedText; switchMode NormalMode ]
            | VisualModes, RemappedMatches layout [ "D" ] _ -> [ run Delete EndOfLine; switchMode NormalMode ]
            | VisualModes, RemappedMatches layout [ "c" ] _
            | VisualModes, RemappedMatches layout [ "s" ] _ -> [ run Change SelectedText ]
            | VisualModes, RemappedMatches layout [ "o" ] _ -> [ run SelectionOtherEnd Nothing ]
            | NormalMode, [ "~" ] -> [ run ToggleCase CurrentLocation ]
            | VisualModes, [ "~" ] -> [ run ToggleCase SelectedText; switchMode NormalMode ]
            | VisualModes, RemappedMatches layout [ "y" ] _ -> [ run (Yank EmptyRegister) SelectedText; switchMode NormalMode ]
            | VisualModes, RemappedMatches layout [ "Y" ] _ -> [ run (Yank EmptyRegister) WholeLine; switchMode NormalMode ]
            | VisualModes, RemappedMatches layout [ ">" ] _ -> [ run (EditorFunc EditActions.IndentSelection) Nothing; switchMode NormalMode ]
            | VisualModes, RemappedMatches layout [ "<" ] _ -> [ run (EditorFunc EditActions.UnIndentSelection) Nothing; switchMode NormalMode ]
            | VisualModes, RemappedMatches layout [ "=" ] _ -> [ run EqualIndent SelectedText; switchMode NormalMode ]
            | NotInsertMode, RemappedMatches layout [ "Z" ] _ -> wait
            | NotInsertMode, RemappedMatches layout  ["Z"; "Z" ] _ -> [ func Window.closeTab ]
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
            | NotInsertMode, RemappedMatches layout [ "g"; "p" ] _ -> wait
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "s" ] _ -> [ gotoPad "ProjectPad" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "c" ] _ -> [ gotoPad "ClassPad" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "e" ] _ -> [ gotoPad "MonoDevelop.Ide.Gui.Pads.ErrorListPad" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "t" ] _ -> [ gotoPad "MonoDevelop.Ide.Gui.Pads.TaskListPad" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "p" ] _ -> [ gotoPad "MonoDevelop.DesignerSupport.PropertyPad" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "o" ] _ -> [ gotoPad "MonoDevelop.DesignerSupport.DocumentOutlinePad" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "b" ] _ -> [ gotoPad "MonoDevelop.Debugger.BreakpointPad" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "l" ] _ -> [ gotoPad "MonoDevelop.Debugger.LocalsPad" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "w" ] _ -> [ gotoPad "MonoDevelop.Debugger.WatchPad" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "i" ] _ -> [ gotoPad "MonoDevelop.Debugger.ImmediatePad" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "n" ] _ -> [ gotoPad "MonoDevelop.FSharp.FSharpInteractivePad" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "f" ] _ ->
                let searchResultPads = IdeApp.Workbench.Pads |> Seq.filter(fun p -> p.Content :? MonoDevelop.Ide.FindInFiles.SearchResultPad)
                match searchResultPads |> Seq.length with
                | 0 -> resetKeys
                | 1 -> [ gotoPad "SearchPad - Search Results - 0" ]
                | _ -> wait
            | NotInsertMode, [ RemappedMatchesChar layout "g" _; RemappedMatchesChar layout "p" _; RemappedMatchesChar layout "f" _; OneToNine d ] ->
                [ gotoPad (sprintf "SearchPad - Search Results - %d" (d-1)) ]
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "d" ] _ -> wait
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "d"; "t" ] _ -> [ gotoPad "MonoDevelop.Debugger.ThreadsPad" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "d"; "s" ] _ -> [ gotoPad "MonoDevelop.Debugger.StackTracePad" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "d"; "c" ] _ -> [ gotoPad "MonoDevelop.Debugger.StackTracePad" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "u" ] _ -> wait
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "u"; "t" ] _ -> [ gotoPad "MonoDevelop.UnitTesting.TestPad" ]
            | NotInsertMode, RemappedMatches layout [ "g"; "p"; "u"; "r" ] _-> [ gotoPad "MonoDevelop.UnitTesting.TestResultsPad" ]
            | _, [] when numericArgument.IsSome  -> wait
            | _ -> resetKeys
        action, newState

    let keyPressToVimKey (keyPress:KeyDescriptor) =
        match keyPress.KeyChar with
        | c when keyPress.ModifierKeys = ModifierKeys.Control ->
            Control c
        | c when keyPress.ModifierKeys = ModifierKeys.Command ->
            Super c
        | 'z' when keyPress.ModifierKeys = ModifierKeys.Command ->
            Key 'u'
        | c when keyPress.KeyChar <> '\000' ->
            Key c
        | _ ->
            match keyPress.SpecialKey with
            | SpecialKey.Escape -> Esc
            | SpecialKey.Return -> Ret
            | SpecialKey.Left -> VimKey.Left
            | SpecialKey.Down -> VimKey.Down
            | SpecialKey.Up -> VimKey.Up
            | SpecialKey.Right -> VimKey.Right
            | SpecialKey.BackSpace -> Backspace
            | SpecialKey.Delete -> VimKey.Delete
            | _ -> Key keyPress.KeyChar

    let handleKeyPress state (keyPress:KeyDescriptor) (editor:TextEditor) config =
        let fileName = editor.FileName
        let vimKey =
            match state.mode, keyPress.KeyChar, config.insertModeEscapeKey with
            | InsertMode, c, Some combo when (string c) = combo.insertModeEscapeKey1 ->
                EscapeKey c
            | InsertMode, c, Some combo when state.keys |> List.tryHead = Some (Key (char combo.insertModeEscapeKey1)) ->
                EscapeKey c
            | _ -> keyPressToVimKey keyPress

        let newState = { state with keys = state.keys @ [ vimKey ] }
        let action, newState = parseKeys newState config
        let newState =
            match state.statusMessage, state.mode, newState.mode with
            | Some _, NormalMode, NormalMode -> { newState with statusMessage = None }
            | _ -> newState

        LoggingService.LogDebug (sprintf "%A" action)

        let rec performActions config actions' state handled =
            match actions' with
            | [] -> state, handled
            | [ only ] ->
                match only.commandType with
                | DoNothing -> state, true
                | _ ->
                    let newState = runCommand config state editor only
                    { newState with keys = [] }, true
            | h::t ->
                let newState = runCommand config state editor h
                performActions config t newState true

        let newState, handled =
            let processKey() =
                use group = editor.OpenUndoGroup()
                state.macro
                |> Option.iter(fun (Macro c) -> macros.[c] <- macros.[c] @ action)
                performActions config action newState false

            match state.mode, newState.keys |> List.map string with
            | ExMode _, [ Escape ] -> processKey()
            | ExMode _, _ ->
                let state, actions = exMode.processKey state keyPress
                performActions config actions state true
            | _ -> processKey()

        let firstAction = action |> List.head

        let newState =
            match config.keyboardLayout, state.mode, vimKey, firstAction.commandType with
            | _, InsertMode, _, _ ->
                newState.macro |> Option.iter(fun (Macro m) ->
                    macros.[m] <- macros.[m] @ [ typeChar vimKey ])
                //{ newState with lastAction = newState.lastAction @ [ typeChar vimKey ]}
                newState
            | _, NotInsertMode, _, SwitchMode VisualModes
            | _, NotInsertMode, _, Delete
            | _, NotInsertMode, _, Change
            | _, NotInsertMode, _, Indent
            | _, NotInsertMode, _, UnIndent
            | _, NotInsertMode, _, Put _
            | _, NotInsertMode, _, ReplaceChar _
            | Qwerty, NotInsertMode, Key 'a', _
            | Qwerty, NotInsertMode, Key 'i', _
            | Qwerty, NotInsertMode, Key 'I', _
            | Qwerty, NotInsertMode, Key 'o', _
            | Qwerty, NotInsertMode, Key 'O', _
            | Qwerty, NotInsertMode, Key 'A', _
            | Colemak, NotInsertMode, Key 'a', _
            | Colemak, NotInsertMode, Key 'u', _
            | Colemak, NotInsertMode, Key 'U', _
            | Colemak, NotInsertMode, Key 'y', _
            | Colemak, NotInsertMode, Key 'Y', _
            | Colemak, NotInsertMode, Key 'A', _
            | Dvorak, NotInsertMode, Key 'a', _
            | Dvorak, NotInsertMode, Key 'c', _
            | Dvorak, NotInsertMode, Key 'C', _
            | Dvorak, NotInsertMode, Key 'r', _
            | Dvorak, NotInsertMode, Key 'R', _
            | Dvorak, NotInsertMode, Key 'A', _-> { newState with lastAction = action }
            | _ -> newState

        editorStates.[fileName] <- newState
        newState, handled

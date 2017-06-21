namespace XSVim
open System

type BeforeOrAfter = Before | After | OverSelection

type CaretMode = Insert | Block

type Selection = {
    linewise : bool
    content: string
}

type Register = 
    | Register of Char
    | EmptyRegister

type VimMode =
    | NormalMode
    | VisualMode
    | VisualBlockMode
    | VisualLineMode
    | InsertMode
    | ExMode

type CommandType =
    | Move
    | Visual
    | Yank of Register
    | Put of BeforeOrAfter
    | Delete
    | DeleteWholeLines
    | DeleteLeft
    | BlockInsert of BeforeOrAfter
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
    | Star of BeforeOrAfter
    | ToggleCase
    | InsertChar of string
    | IncrementNumber
    | DecrementNumber
    | SetMark of string
    | IncrementalSearch of string
    | IncrementalSearchBackwards of string

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
    | AQuotedBlock of char
    | InnerQuotedBlock of char
    | WholeLine
    | WholeLineIncludingDelimiter
    | LastLine
    | FirstVisibleLine
    | MiddleVisibleLine
    | LastVisibleLine
    // motions
    | Up
    | Down
    | Left
    | Right
    | RightIncludingDelimiter
    | FirstNonWhitespace
    | StartOfLine
    | StartOfLineNumber of int
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
    | SelectedText
    | SelectionStart
    | MatchingBrace
    | ToMark of Marker
    | Offset of int
    | ToSearch of string
    | ToSearchBackwards of string
    | SearchAgain
    | SearchAgainBackwards

type VimAction = {
    repeat: int option
    commandType: CommandType
    textObject: TextObject
}

type VimSelection = { start: int; finish: int; mode: VimMode }

type VimState = {
    keys: string list
    mode: VimMode
    visualStartOffset: int
    lastSelection: VimSelection option
    findCharCommand: VimAction option // f,F,t or T command to be repeated with ;
    lastAction: VimAction list // used by . command to repeat the last action
    desiredColumn: int option
    undoGroup: IDisposable option
    statusMessage: string option
    lastSearch: string option // Last term searched for with / or ?
}

// shim for the build server which runs Mono 4.6.1
module Option =
    let inline defaultValue value = 
        function
        | Some v -> v
        | None -> value

[<AutoOpen>]
module commandHelpers =
    let getCommand repeat commandType textObject =
        { repeat=repeat; commandType=commandType; textObject=textObject }

    let runOnce = getCommand (Some 1)
    let typeChar c = runOnce (InsertChar c) Nothing
    let wait = [ getCommand None DoNothing Nothing ]
    let switchMode mode = runOnce (SwitchMode mode) Nothing
    let dispatch command = runOnce (Dispatch command) Nothing
    let resetKeys = [ runOnce ResetKeys Nothing ]

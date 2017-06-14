namespace XSVim
open System

type BeforeOrAfter = Before | After | OverSelection

type CaretMode = Insert | Block

type MarkLocation = {
    offset: int
    fileName: string
}

type Selection = {
    linewise : bool
    content: string
}

type Register = 
    |Register of Char
    |EmptyRegister

type VimMode =
    | NormalMode
    | VisualMode
    | VisualBlockMode
    | VisualLineMode
    | InsertMode

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
    | ToMark of MarkLocation

type VimAction = {
    repeat: int option
    commandType: CommandType
    textObject: TextObject
}

type VimState = {
    keys: string list
    mode: VimMode
    visualStartOffset: int
    findCharCommand: VimAction option // f,F,t or T command to be repeated with ;
    lastAction: VimAction list // used by . command to repeat the last action
    desiredColumn: int option
    undoGroup: IDisposable option
    statusMessage: string option
}

// shim for the build server which runs Mono 4.6.1
module Option =
    let inline defaultValue value = 
        function
        | Some v -> v
        | None -> value

namespace XSVim
open System

type BeforeOrAfter = Before | After | OverSelection

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
    | DeleteWholeLines
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
    | Star of BeforeOrAfter
    | ToggleCase

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
    | WholeLineToEndOfDocument
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

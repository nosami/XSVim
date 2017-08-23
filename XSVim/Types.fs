namespace XSVim
open System
open MonoDevelop.Ide
open MonoDevelop.Ide.Editor

type BeforeOrAfter = Before | After | OverSelection

type CaretMode = Insert | Block

type Selection = {
    linewise : bool
    content: string
}

type Register = 
    | Register of char
    | EmptyRegister
   

type VimMode =
    | NormalMode
    | VisualMode
    | VisualBlockMode
    | VisualLineMode
    | InsertMode
    | ExMode of char // initial char typed to get to command line

type CommandType =
    | Move
    | Visual
    | Yank of Register
    | Put of BeforeOrAfter
    | Delete
    | Substitute
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
    | SetSearchAction of CommandType
    | MacroStart of char
    | MacroEnd
    | ReplayMacro of char
    | NextTab
    | PreviousTab
    | Func of (TextEditor -> unit)

type MoveRightBehaviour = StopAtEndOfLine | MoveToNextLineAtEnd

type Jump =
    | StartOfLineNumber of int
    | StartOfDocument
    | ToMark of Marker
    | ToSearch of string
    | ToSearchBackwards of string
    | SearchAgain
    | SearchAgainBackwards
    | HalfPageUp
    | HalfPageDown
    | PageUp
    | PageDown
    | LastLine
    | FirstVisibleLine
    | MiddleVisibleLine
    | LastVisibleLine
    | ParagraphForwards
    | ParagraphBackwards

type TextObject =
    | Jump of Jump
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
    // motions
    | Up
    | Down
    | Left
    | Right of MoveRightBehaviour
    | RightIncludingDelimiter
    | FirstNonWhitespace
    | StartOfLine
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
    | ForwardToEndOfWord
    | ForwardToEndOfWORD
    | BackwardToEndOfWord
    | BackwardToEndOfWORD
    | Nothing
    | CurrentLocation
    | SelectedText
    | SelectionStart
    | MatchingBrace
    | Offset of int

type VimAction = {
    repeat: int option
    commandType: CommandType
    textObject: TextObject
}

type Macro = Macro of char

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
    searchAction: CommandType option // Delete, Change, Visual, Yank or Move when / or ? is pressed
    lastSearch: TextObject option // Last term searched for with / or ?
    macro: Macro option
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
    let func f = runOnce (Func f) Nothing

    let dispatchCommand command = IdeApp.CommandService.DispatchCommand command |> ignore

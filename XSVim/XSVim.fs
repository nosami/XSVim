namespace XSVim

open System
open System.Globalization
open MonoDevelop.Ide.Editor
open MonoDevelop.Ide.Editor.Extension

type BlockChar = 
    | LeftSquare = '[' 
    | RightSquare = ']'
    | LeftParens = '('
    | RightParens = ')'
    | LeftBrace = '{'
    | RightBrace = '}'
    | LeftChevron = '<'
    | RightChevron = '>'

type CommandType =
    | Move
    | Select
    | Delete
    | Change
    | DoNothing

type Repeater =
    | Repeat of int

//type Motion =
  
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
    | ABlock of c:BlockChar
    | InnerBlock of c:BlockChar
    | WholeLine
    // motions
    | Up
    | Down
    | Left
    | Right
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

type VimAction = {
    repeat: int
    commandType: CommandType
    textObject: TextObject
}

type XSVim() =
    inherit TextEditorExtension()

    let (|Digit|_|) character =
        if Char.IsDigit character then
            Some (CharUnicodeInfo.GetDecimalDigitValue character)
        else
            None

    let (|Movement|_|) character =
        match character with
        | 'h' -> Some Left
        | 'j' -> Some Down
        | 'k' -> Some Up
        | 'l' -> Some Right
        | _ -> None

    let (|Action|_|) character =
        match character with
        | 'd' -> Some Delete
        | 'c' -> Some Change
        | _ -> None

    let keys = ResizeArray<_>()

    let getCommand (repeat: int option) commandType textObject =
        Some { repeat=(match repeat with | Some r -> r | None -> 1); commandType=commandType; textObject=textObject }

    //let doOnce = getAction 1
    //let doTimes n = getAction n

    override x.Initialize() = ()

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
            | Digit d1 :: Digit d2 :: Digit d3 :: Digit d4 :: t -> Some(d1 * 1000 + d2 * 100 + d3 * 10 + d4), t
            | Digit d1 :: Digit d2 :: Digit d3 :: t -> Some (d1 * 100 + d2 * 10 + d3), t
            | Digit d1 :: Digit d2 :: t -> Some (d1 * 10 + d2), t
            | Digit d :: t -> Some d,t
            //| [Digit d] -> Some d, []
            | _ -> None, keyList

        let action =
            match keyList with
            | [ Movement m ] -> getCommand multiplier Move m
            //| [ Digit d; Movement m ] -> doTimes d Move m
            | [ Action action; 'd' ] -> getCommand multiplier action WholeLine
            //| [ Digit d; Action action; 'd'] -> doTimes d Delete WholeLine
            | [ Action action; 't'; c ] -> getCommand multiplier action (ToCharInclusive c)
            | [ Action action; 'f'; c ] -> getCommand multiplier action (ToCharExclusive c)
            | _ -> None

        match multiplier, action with
        | _, Some action' ->
            MonoDevelop.Core.LoggingService.LogDebug (sprintf "%A" action')
            keys.Clear()
            false
        | None, None -> base.KeyPress descriptor
        | _, _ -> false

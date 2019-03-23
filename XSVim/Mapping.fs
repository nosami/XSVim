﻿namespace XSVim

[<AutoOpen>]
module mapping =
    let (|KeyPressColemak|_|) = function
        | "f" -> Some "e"
        | "p" -> Some "r"
        | "g" -> Some "t"
        | "j" -> Some "y"
        | "l" -> Some "u"
        | "u" -> Some "i"
        | "y" -> Some "o"
        | ";" -> Some "p"
        | "r" -> Some "s"
        | "s" -> Some "d"
        | "t" -> Some "f"
        | "d" -> Some "g"
        | "n" -> Some "j"
        | "e" -> Some "k"
        | "i" -> Some "l"
        | "o" -> Some ";"
        | "k" -> Some "n"
        | "F" -> Some "E"
        | "P" -> Some "R"
        | "G" -> Some "T"
        | "J" -> Some "Y"
        | "L" -> Some "U"
        | "U" -> Some "I"
        | "Y" -> Some "O"
        | ":" -> Some "P"
        | "R" -> Some "S"
        | "S" -> Some "D"
        | "T" -> Some "F"
        | "D" -> Some "G"
        | "N" -> Some "J"
        | "E" -> Some "K"
        | "I" -> Some "L"
        | "O" -> Some ":"
        | "K" -> Some "N"
        | _ -> None

    let remap layout key =
        match layout, key with
        | Colemak, KeyPressColemak c -> c
        | _, c -> c

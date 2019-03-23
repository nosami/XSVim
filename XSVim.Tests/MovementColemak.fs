namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Movement tests Colemak`` =
    [<Test>]
    let ``Move to next word``() =
        assertColemakText "aa$a bbb" "w" "aaa b$bb"

    [<Test>]
    let ``Move to next word on next line``() =
        assertColemakText "aa$a\n  bbb" "w" "aaa\n  b$bb"

    [<Test>]
    let ``Move to empty line``() =
        assertColemakText "aa$a\n\nbbb" "w" "aaa\n\n$bbb"

    [<Test>]
    let ``Moves to EOF when there is no next word``() =
        assertColemakText "aa$aa" "w" "aaaa$"

    [<Test>]
    let ``w skips over tabs``() =
        assertColemakText "\t$\t\taaaa" "w" "\t\t\ta$aaa"

    [<Test>]
    let ``Move to word end``() =
        assertColemakText "aa$a bbb" "f" "aaa$ bbb"

    [<Test>]
    let ``Move to next word end``() =
        assertColemakText "aaa$ bbb" "f" "aaa bbb$"

    [<Test>]
    let ``Move to second word end``() =
        assertColemakText "aa$a bbb" "ff" "aaa bbb$"

    [<Test>]
    let ``e jumps over punctuation``() =
        assertColemakText "int model$);\n " "f" "int model);$\n "

    [<Test>]
    let ``e jumps over chevrons``() =
        assertColemakText "Task<List<SomeWord$>> nextWord" "f" "Task<List<SomeWord>>$ nextWord"

    [<Test>]
    let ``e jumps from chevron to end of next word``() =
        assertColemakText "Task<List<SomeWord>>$ nextWord" "f" "Task<List<SomeWord>> nextWord$"

    [<Test>]
    let ``e jumps over spaces``() =
        assertColemakText " $  abcde" "f" "   abcde$"

    [<Test>]
    let ``e stops before dot``() =
        assertColemakText "open$ System.Collections.Generic" "f" "open System$.Collections.Generic"

    [<Test>]
    let ``Move to end of line``() =
        assertColemakText "aa$a aaa\nbbb" "$" "aaa aaa$\nbbb"

    [<Test>]
    let ``Move to end of document``() =
        assertColemakText "aa$aaaa\nbbbbbb" "D" "aaaaaa\nb$bbbbb"

    [<Test>]
    let ``Move to start of document``() =
        assertColemakText "aaaaaa\nbb$bbbb" "dd" "a$aaaaa\nbbbbbb"

    [<Test>]
    let ``Move to line 2``() =
        assertColemakText "a$aaaaa\n  bbbbbb" "2dd" "aaaaaa\n  b$bbbbb"

    [<Test>]
    let ``Move to line 3``() =
        assertColemakText "a$aaaaa\nbbbbbb\ncccccc\ndddddd" "3D" "aaaaaa\nbbbbbb\nc$ccccc\ndddddd"

    [<Test>]
    let ``Move down to desired column``() =
        assertColemakText "12345$6\n123\n123456" "nn" "123456\n123\n12345$6"

    [<Test>]
    let ``Move down to last column``() =
        assertColemakText "12345$6\n123\n123456" "n" "123456\n123$\n123456"

    [<Test>]
    let ``Move across then down``() =
        assertColemakText "1$2\n12\n" "in" "12\n12$\n"

    [<Test>]
    let ``Move ten right``() =
        assertColemakText "a$bcdefghijkl" "10i" "abcdefghijk$l"

    [<Test>]
    let ``Does not move right past delimiter``() =
        assertColemakText "a$b\n" "ii" "ab$\n"

    [<Test>]
    let ``Find moves to digit``() =
        assertColemakText "abc$ d1 d2 d3" "t2" "abc d1 d2$ d3"

    [<Test>]
    let ``Reverse find moves to digit``() =
        assertColemakText "abc d1 d2 d$3" "T1" "abc d1$ d2 d3"

    [<Test>]
    let ``Find moves to mapped character``() =
        assertColemakText "abc$ t1 f2 d3" "tt" "abc t$1 f2 d3"

    [<Test>]
    let ``Reverse find moves to mapped character``() =
        assertColemakText "abc t1 f2 d$3" "Tt" "abc t$1 f2 d3"

    [<Test>]
    let ``Till moves to digit``() =
        assertColemakText "abc$ d1 d2 d3" "g2" "abc d1 d$2 d3"

    [<Test>]
    let ``Reverse till moves to digit``() =
        assertColemakText "abc d1 d2 d$3" "G1" "abc d1 $d2 d3"

    [<Test>]
    let ``Till moves to mapped char``() =
        assertColemakText "abc$ t1 f2 d3" "gt" "abc $t1 f2 d3"

    [<Test>]
    let ``2fd moves to second d``() =
        assertColemakText "abc$ d1 d2 d3" "2td" "abc d1 d$2 d3"

    [<Test>]
    let ``F finds previous char``() =
        assertColemakText "a a$" "Ta" "a$ a"

    [<Test>]
    let ``f is repeatable with ;``() =
        assertColemakText " $ a1 a2" "tao" "  a1 a$2"

    [<Test>]
    let ``f is reversed with ,``() =
        assertColemakText " $ a1 a2" "tao," "  a$1 a2"

    [<Test>]
    let ``t does not move if caret is already just before search char``() =
        assertColemakText " $a1 a2" "ga" " $a1 a2"

    [<Test>]
    let ``T does not move if caret is already just after search char``() =
        assertColemakText "a1 a2$" "Ga" "a1 a2$"

    [<Test>]
    let ``t is repeatable with ;``() =
        assertColemakText " $a1 a2" "gao" " a1 $a2"

    [<Test>]
    let ``T is repeatable with ;``() =
        assertColemakText "a1 a2$" "Gao" "a1$ a2"

    [<Test>]
    let ``ge moves back to end of last word``() =
        assertColemakText "abc de$f" "df" "abc$ def"

    [<Test>]
    let ``ge between words moves back to end of last word``() =
        assertColemakText "abc  $def" "df" "abc$  def"

    [<Test>]
    let ``ge stops at first character``() =
        assertColemakText "abc$" "df" "a$bc"

    [<Test>]
    let ``gE moves back to end of last WORD``() =
        assertColemakText "abc def.gh$i" "dF" "abc$ def.ghi"

    [<Test>]
    let ``l stops at EOL``() =
        assertColemakText "abc$\ndef" "i" "abc$\ndef"

    [<Test>]
    let ``space moves past EOL``() =
        assertColemakText "abc$\ndef" " " "abc\nd$ef"

    [<Test>]
    let ``% moves to matching parens``() =
        assertColemakText "($foo(bar))" "%" "(foo(bar))$"

    [<Test>]
    let ``[{ goes to previous unmatched {``() =
        assertColemakText "func { case { a } case$ { b } }" "[{" "func {$ case { a } case { b } }"

    [<Test>]
    let ``[( goes to previous unmatched (``() =
        assertColemakText "if (a == (b)c$)" "[(" "if ($a == (b)c)"

    [<Test>]
    let ``]} goes to next unmatched }``() =
        assertColemakText "func { case$ { a } case { b } }" "]}" "func { case { a } case { b } }$"

    [<Test>]
    let ``]) goes to next unmatched )``() =
        assertColemakText "if (a$ == (b)c)" "])" "if (a == (b)c)$"
        
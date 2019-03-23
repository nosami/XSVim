namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Movement tests Dvorak`` =
    [<Test>]
    let ``Move to next word``() =
        assertDvorakText "aa$a bbb" "," "aaa b$bb"

    [<Test>]
    let ``Move to next word on next line``() =
        assertDvorakText "aa$a\n  bbb" "," "aaa\n  b$bb"

    [<Test>]
    let ``Move to empty line``() =
        assertDvorakText "aa$a\n\nbbb" "," "aaa\n\n$bbb"

    [<Test>]
    let ``Moves to EOF when there is no next word``() =
        assertDvorakText "aa$aa" "," "aaaa$"

    [<Test>]
    let ``w skips over tabs``() =
        assertDvorakText "\t$\t\taaaa" "," "\t\t\ta$aaa"

    [<Test>]
    let ``Move to word end``() =
        assertDvorakText "aa$a bbb" "." "aaa$ bbb"

    [<Test>]
    let ``Move to next word end``() =
        assertDvorakText "aaa$ bbb" "." "aaa bbb$"

    [<Test>]
    let ``Move to second word end``() =
        assertDvorakText "aa$a bbb" ".." "aaa bbb$"

    [<Test>]
    let ``e jumps over punctuation``() =
        assertDvorakText "int model$);\n " "." "int model);$\n "

    [<Test>]
    let ``e jumps over chevrons``() =
        assertDvorakText "Task<List<SomeWord$>> nextWord" "." "Task<List<SomeWord>>$ nextWord"

    [<Test>]
    let ``e jumps from chevron to end of next word``() =
        assertDvorakText "Task<List<SomeWord>>$ nextWord" "." "Task<List<SomeWord>> nextWord$"

    [<Test>]
    let ``e jumps over spaces``() =
        assertDvorakText " $  abcde" "." "   abcde$"

    [<Test>]
    let ``e stops before dot``() =
        assertDvorakText "open$ System.Collections.Generic" "." "open System$.Collections.Generic"

    [<Test>]
    let ``Move to end of line``() =
        assertDvorakText "aa$a aaa\nbbb" "$" "aaa aaa$\nbbb"

    [<Test>]
    let ``Move to end of document``() =
        assertDvorakText "aa$aaaa\nbbbbbb" "I" "aaaaaa\nb$bbbbb"

    [<Test>]
    let ``Move to start of document``() =
        assertDvorakText "aaaaaa\nbb$bbbb" "ii" "a$aaaaa\nbbbbbb"

    [<Test>]
    let ``Move to line 2``() =
        assertDvorakText "a$aaaaa\n  bbbbbb" "2ii" "aaaaaa\n  b$bbbbb"

    [<Test>]
    let ``Move to line 3``() =
        assertDvorakText "a$aaaaa\nbbbbbb\ncccccc\ndddddd" "3I" "aaaaaa\nbbbbbb\nc$ccccc\ndddddd"

    [<Test>]
    let ``Move down to desired column``() =
        assertDvorakText "12345$6\n123\n123456" "hh" "123456\n123\n12345$6"

    [<Test>]
    let ``Move down to last column``() =
        assertDvorakText "12345$6\n123\n123456" "h" "123456\n123$\n123456"

    [<Test>]
    let ``Move across then down``() =
        assertDvorakText "1$2\n12\n" "nh" "12\n12$\n"

    [<Test>]
    let ``Move ten right``() =
        assertDvorakText "a$bcdefghijkl" "10n" "abcdefghijk$l"

    [<Test>]
    let ``Does not move right past delimiter``() =
        assertDvorakText "a$b\n" "nn" "ab$\n"

    [<Test>]
    let ``Find moves to digit``() =
        assertDvorakText "abc$ d1 d2 d3" "u2" "abc d1 d2$ d3"

    [<Test>]
    let ``Reverse find moves to digit``() =
        assertDvorakText "abc d1 d2 d$3" "U1" "abc d1$ d2 d3"

    [<Test>]
    let ``Till moves to digit``() =
        assertDvorakText "abc$ d1 d2 d3" "y2" "abc d1 d$2 d3"

    [<Test>]
    let ``Reverse till moves to digit``() =
        assertDvorakText "abc d1 d2 d$3" "Y1" "abc d1 $d2 d3"

    [<Test>]
    let ``2fd moves to second d``() =
        assertDvorakText "abc$ d1 d2 d3" "2ud" "abc d1 d$2 d3"

    [<Test>]
    let ``F finds previous char``() =
        assertDvorakText "a a$" "Ua" "a$ a"

    [<Test>]
    let ``f is repeatable with ;``() =
        assertDvorakText " $ a1 a2" "uas" "  a1 a$2"

    [<Test>]
    let ``f is reversed with ,``() =
        assertDvorakText " $ a1 a2" "uasw" "  a$1 a2"

    [<Test>]
    let ``t does not move if caret is already just before search char``() =
        assertDvorakText " $a1 a2" "ya" " $a1 a2"

    [<Test>]
    let ``T does not move if caret is already just after search char``() =
        assertDvorakText "a1 a2$" "Ya" "a1 a2$"

    [<Test>]
    let ``t is repeatable with ;``() =
        assertDvorakText " $a1 a2" "yas" " a1 $a2"

    [<Test>]
    let ``T is repeatable with ;``() =
        assertDvorakText "a1 a2$" "Yas" "a1$ a2"

    [<Test>]
    let ``ge moves back to end of last word``() =
        assertDvorakText "abc de$f" "i." "abc$ def"

    [<Test>]
    let ``ge between words moves back to end of last word``() =
        assertDvorakText "abc  $def" "i." "abc$  def"

    [<Test>]
    let ``ge stops at first character``() =
        assertDvorakText "abc$" "i." "a$bc"

    [<Test>]
    let ``gE moves back to end of last WORD``() =
        assertDvorakText "abc def.gh$i" "i>" "abc$ def.ghi"

    [<Test>]
    let ``l stops at EOL``() =
        assertDvorakText "abc$\ndef" "n" "abc$\ndef"

    [<Test>]
    let ``space moves past EOL``() =
        assertDvorakText "abc$\ndef" " " "abc\nd$ef"

    [<Test>]
    let ``% moves to matching parens``() =
        assertDvorakText "($foo(bar))" "%" "(foo(bar))$"

    [<Test>]
    let ``[{ goes to previous unmatched {``() =
        assertDvorakText "func { case { a } case$ { b } }" "/?" "func {$ case { a } case { b } }"

    [<Test>]
    let ``[( goes to previous unmatched (``() =
        assertDvorakText "if (a == (b)c$)" "/(" "if ($a == (b)c)"

    [<Test>]
    let ``]} goes to next unmatched }``() =
        assertDvorakText "func { case$ { a } case { b } }" "=+" "func { case { a } case { b } }$"

    [<Test>]
    let ``]) goes to next unmatched )``() =
        assertDvorakText "if (a$ == (b)c)" "=)" "if (a == (b)c)$"

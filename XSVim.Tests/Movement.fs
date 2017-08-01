namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Movement tests`` =
    [<Test>]
    let ``Move to next word``() =
        assertText "aa$a bbb" "w" "aaa b$bb"

    [<Test>]
    let ``Move to next word on next line``() =
        assertText "aa$a\n  bbb" "w" "aaa\n  b$bb"

    [<Test>]
    let ``Move to empty line``() =
        assertText "aa$a\n\nbbb" "w" "aaa\n\n$bbb"

    [<Test>]
    let ``Moves to EOF when there is no next word``() =
        assertText "aa$aa" "w" "aaaa$"

    [<Test>]
    let ``Move to word end``() =
        assertText "aa$a bbb" "e" "aaa$ bbb"

    [<Test>]
    let ``Move to next word end``() =
        assertText "aaa$ bbb" "e" "aaa bbb$"

    [<Test>]
    let ``Move to second word end``() =
        assertText "aa$a bbb" "ee" "aaa bbb$"

    [<Test>]
    let ``e jumps over punctuation``() =
        assertText "int model$);\n " "e" "int model);$\n "

    [<Test>]
    let ``e jumps over chevrons``() =
        assertText "Task<List<SomeWord$>> nextWord" "e" "Task<List<SomeWord>>$ nextWord"

    [<Test>]
    let ``e jumps from chevron to end of next word``() =
        assertText "Task<List<SomeWord>>$ nextWord" "e" "Task<List<SomeWord>> nextWord$"

    [<Test>]
    let ``e jumps over spaces``() =
        assertText " $  abcde" "e" "   abcde$"

    [<Test>]
    let ``e stops before dot``() =
        assertText "open$ System.Collections.Generic" "e" "open System$.Collections.Generic"

    [<Test>]
    let ``Move to end of line``() =
        assertText "aa$a aaa\nbbb" "$" "aaa aaa$\nbbb"

    [<Test>]
    let ``Move to end of document``() =
        assertText "aa$aaaa\nbbbbbb" "G" "aaaaaa\nb$bbbbb"

    [<Test>]
    let ``Move to start of document``() =
        assertText "aaaaaa\nbb$bbbb" "gg" "a$aaaaa\nbbbbbb"

    [<Test>]
    let ``Move to line 2``() =
        assertText "a$aaaaa\nbbbbbb" "2gg" "aaaaaa\nb$bbbbb"

    [<Test>]
    let ``Move to line 3``() =
        assertText "a$aaaaa\nbbbbbb\ncccccc\ndddddd" "3G" "aaaaaa\nbbbbbb\nc$ccccc\ndddddd"

    [<Test>]
    let ``Move down to desired column``() =
        assertText "12345$6\n123\n123456" "jj" "123456\n123\n12345$6"

    [<Test>]
    let ``Move down to last column``() =
        assertText "12345$6\n123\n123456" "j" "123456\n123$\n123456"

    [<Test>]
    let ``Move across then down``() =
        assertText "1$2\n12\n" "lj" "12\n12$\n"

    [<Test>]
    let ``Move ten right``() =
        assertText "a$bcdefghijkl" "10l" "abcdefghijk$l"

    [<Test>]
    let ``Does not move right past delimiter``() =
        assertText "a$b\n" "ll" "ab$\n"

    [<Test>]
    let ``Find moves to digit``() =
        assertText "abc$ d1 d2 d3" "f2" "abc d1 d2$ d3"

    [<Test>]
    let ``Reverse find moves to digit``() =
        assertText "abc d1 d2 d$3" "F1" "abc d1$ d2 d3"

    [<Test>]
    let ``Till moves to digit``() =
        assertText "abc$ d1 d2 d3" "t2" "abc d1 d$2 d3"

    [<Test>]
    let ``Reverse till moves to digit``() =
        assertText "abc d1 d2 d$3" "T1" "abc d1 $d2 d3"

    [<Test>]
    let ``2fd moves to second d``() =
        assertText "abc$ d1 d2 d3" "2fd" "abc d1 d$2 d3"

    [<Test>]
    let ``F finds previous char``() =
        assertText "a a$" "Fa" "a$ a"

    [<Test>]
    let ``f is repeatable with ;``() =
        assertText " $ a1 a2" "fa;" "  a1 a$2"

    [<Test>]
    let ``f is reversed with ,``() =
        assertText " $ a1 a2" "fa;," "  a$1 a2"

    [<Test>]
    let ``t does not move if caret is already just before search char``() =
        assertText " $a1 a2" "ta" " $a1 a2"

    [<Test>]
    let ``T does not move if caret is already just after search char``() =
        assertText "a1 a2$" "Ta" "a1 a2$"

    [<Test>]
    let ``t is repeatable with ;``() =
        assertText " $a1 a2" "ta;" " a1 $a2"

    [<Test>]
    let ``T is repeatable with ;``() =
        assertText "a1 a2$" "Ta;" "a1$ a2"

    [<Test>]
    let ``ge moves back to end of last word``() =
        assertText "abc de$f" "ge" "abc$ def"

    [<Test>]
    let ``ge between words moves back to end of last word``() =
        assertText "abc  $def" "ge" "abc$  def"

    [<Test>]
    let ``ge stops at first character``() =
        assertText "abc$" "ge" "a$bc"

    [<Test>]
    let ``gE moves back to end of last WORD``() =
        assertText "abc def.gh$i" "gE" "abc$ def.ghi"
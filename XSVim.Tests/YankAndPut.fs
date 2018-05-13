namespace XSVim.Tests

open XSVim
open NUnit.Framework

[<TestFixture>]
module ``Yank and put tests`` =
    [<Test>]
    let ``Yanking puts cursor at original position before selection was made``() =
        assertText "a$bc" "vly" "a$bc"

    [<Test>]
    let ``Yanking line supports multiplier``() =
        let _  = test "a$bc\ndef\nghi" "2yy"
        Vim.registers.[EmptyRegister].content |> should equal "abc\ndef\n"

    [<Test>]
    let ``Yanking doesn't move caret when there is no selection'``() =
        assertText "a$bcdef" "vll<esc>y$" "abc$def"

    [<Test>]
    let ``Should put line at last line``() =
        assertText "  abc$\ndef" "yyjp" "  abc\ndef\n  a$bc"

    [<Test>]
    let ``Should put ab after``() =
        assertText "a$bc" "vldp" "cab$"

    [<Test>]
    let ``Should put abc over selection in visual mode``() =
        assertText "a$bc" "vlyvllp" "ab$"

    [<Test>]
    let ``P acts like p in visual mode``() =
        assertText "a$bc" "vlyvllP" "ab$"

    [<Test>]
    let ``Can yank into a named register``() =
        let _  = test "ab$cd ef" "\"dyl"
        Vim.registers.[Register 'd'].content |> should equal "b"

    [<Test>]
    let ``yw at the end of a line consumes entire line``()=
        assertText "a$bc" "ywp" "aabc$bc"

    [<Test>]
    let ``Visual line selection should work at EOF``() =
        assertText "123\na$bc" "Vyp" "123\nabc\na$bc"

    [<Test>]
    let ``Single line yank should work at EOF``() =
        assertText "123\na$bc" "yyp" "123\nabc\na$bc"

    [<Test>]
    let ``Line yank should work at EOF``() =
        assertText "abc\nde$f" "yyp" "abc\ndef\nd$ef"

    [<Test>]
    let ``Single line yank containing delimiter``() =
        assertText "1$23\nabc" "yyp" "123\n1$23\nabc"

    [<Test>]
    let ``Linewise put places caret at start of line``() =
        assertText " $  123\n" "yyp" "   123\n   1$23\n"

    [<Test>]
    let ``Linewise Put places caret at start of line``() =
        assertText " $  123\n" "yyP" "   1$23\n   123\n"

    [<Test>]
    let ``Linewise put at EOF places caret at start of line``() =
        assertText "\n $  123" "yyp" "\n   123\n   1$23"

    [<Test>]
    let ``Multi line put places caret at top line of paste``() =
        assertText "aa$a\nbbb\nccc\n" "Vjdp" "ccc\na$aa\nbbb\n"

    [<Test>]
    let ``Multi line put on line without delimiter places caret at top line of paste``() =
        assertText "aa$a\nbbb\nccc" "Vjdp" "ccc\na$aa\nbbb"

    [<Test>]
    let ``x with multiplier stops at EOL``() =
        assertText "ab$cdef\n" "100xp" "abcdef$\n"

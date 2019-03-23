namespace XSVim.Tests

open XSVim
open NUnit.Framework

[<TestFixture>]
module ``Yank and put tests Dvorak`` =
    [<Test>]
    let ``Yanking puts cursor at original position before selection was made``() =
        assertDvorakText "a$bc" "knf" "a$bc"

    [<Test>]
    let ``Yanking line supports multiplier``() =
        let _  = testDvorak "a$bc\ndef\nghi" "2ffy"
        Vim.registers.[EmptyRegister].content |> should equal "abc\ndef\n"

    [<Test>]
    let ``Yanking doesn't move caret when there is no selection'``() =
        assertDvorakText "a$bcdef" "knn<esc>f$" "abc$def"

    [<Test>]
    let ``Should put line at last line``() =
        assertDvorakText "  abc$\ndef" "ffhl" "  abc\ndef\n  a$bc"

    [<Test>]
    let ``Should put ab after``() =
        assertDvorakText "a$bc" "knel" "cab$"

    [<Test>]
    let ``Should put abc over selection in visual mode``() =
        assertDvorakText "a$bc" "knfknnl" "ab$"

    [<Test>]
    let ``P acts like p in visual mode``() =
        assertDvorakText "a$bc" "knfknnL" "ab$"

    [<Test>]
    let ``Can yank into a named register``() =
        let _  = testDvorak "ab$cd ef" "_dfn"
        Vim.registers.[Register 'd'].content |> should equal "b"

    [<Test>]
    let ``yw at the end of a line consumes entire line``()=
        assertDvorakText "a$bc" "f,l" "aabc$bc"

    [<Test>]
    let ``Visual line selection should work at EOF``() =
        assertDvorakText "123\na$bc" "Kfl" "123\nabc\na$bc"

    [<Test>]
    let ``Single line yank should work at EOF``() =
        assertDvorakText "123\na$bc" "ffl" "123\nabc\na$bc"

    [<Test>]
    let ``Line yank should work at EOF``() =
        assertDvorakText "abc\nde$f" "ffl" "abc\ndef\nd$ef"

    [<Test>]
    let ``Single line yank containing delimiter``() =
        assertDvorakText "1$23\nabc" "ffl" "123\n1$23\nabc"

    [<Test>]
    let ``Linewise put places caret at start of line``() =
        assertDvorakText " $  123\n" "ffl" "   123\n   1$23\n"

    [<Test>]
    let ``Linewise Put places caret at start of line``() =
        assertDvorakText " $  123\n" "ffL" "   1$23\n   123\n"

    [<Test>]
    let ``Linewise put at EOF places caret at start of line``() =
        assertDvorakText "\n $  123" "ffl" "\n   123\n   1$23"

    [<Test>]
    let ``Multi line put places caret at top line of paste``() =
        assertDvorakText "aa$a\nbbb\nccc\n" "Khel" "ccc\na$aa\nbbb\n"

    [<Test>]
    let ``Multi line put on line without delimiter places caret at top line of paste``() =
        assertDvorakText "aa$a\nbbb\nccc" "Khel" "ccc\na$aa\nbbb"

    [<Test>]
    let ``x with multiplier stops at EOL``() =
        assertDvorakText "ab$cdef\n" "100ql" "abcdef$\n"

    [<Test>]
    let ``2dw yank two words``() =
        assertDvorakText "p$ublic int someInt = 1;" "2e,L" "public int s$omeInt = 1;"

    [<Test>]
    let ``2d} yank two paragraphs``() =
        let _  = testDvorak " $\na\n\nb\n" "2e+"
        Vim.registers.[EmptyRegister].content |> should equal " \na\n\nb\n"

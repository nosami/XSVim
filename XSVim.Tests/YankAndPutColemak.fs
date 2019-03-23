namespace XSVim.Tests

open XSVim
open NUnit.Framework

[<TestFixture>]
module ``Yank and put tests Colemak`` =
    [<Test>]
    let ``Yanking puts cursor at original position before selection was made``() =
        assertColemakText "a$bc" "vij" "a$bc"

    [<Test>]
    let ``Yanking line supports multiplier``() =
        let _  = testColemak "a$bc\ndef\nghi" "2jj"
        Vim.registers.[EmptyRegister].content |> should equal "abc\ndef\n"

    [<Test>]
    let ``Yanking doesn't move caret when there is no selection'``() =
        assertColemakText "a$bcdef" "vii<esc>j$" "abc$def"

    [<Test>]
    let ``Should put line at last line``() =
        assertColemakText "  abc$\ndef" "jjn;" "  abc\ndef\n  a$bc"

    [<Test>]
    let ``Should put ab after``() =
        assertColemakText "a$bc" "vis;" "cab$"

    [<Test>]
    let ``Should put abc over selection in visual mode``() =
        assertColemakText "a$bc" "vivjvii;" "ab$"

    [<Test>]
    let ``P acts like p in visual mode``() =
        assertColemakText "a$bc" "vijvii:" "ab$"

    [<Test>]
    let ``Can yank into a named register``() =
        let _  = testColemak "ab$cd ef" "\"dji"
        Vim.registers.[Register 'd'].content |> should equal "b"

    [<Test>]
    let ``yw at the end of a line consumes entire line``()=
        assertColemakText "a$bc" "jw;" "aabc$bc"

    [<Test>]
    let ``Visual line selection should work at EOF``() =
        assertColemakText "123\na$bc" "Vj;" "123\nabc\na$bc"

    [<Test>]
    let ``Single line yank should work at EOF``() =
        assertColemakText "123\na$bc" "jj;" "123\nabc\na$bc"

    [<Test>]
    let ``Line yank should work at EOF``() =
        assertColemakText "abc\nde$f" "jj;" "abc\ndef\nd$ef"

    [<Test>]
    let ``Single line yank containing delimiter``() =
        assertColemakText "1$23\nabc" "jj;" "123\n1$23\nabc"

    [<Test>]
    let ``Linewise put places caret at start of line``() =
        assertColemakText " $  123\n" "jj;" "   123\n   1$23\n"

    [<Test>]
    let ``Linewise Put places caret at start of line``() =
        assertColemakText " $  123\n" "jj:" "   1$23\n   123\n"

    [<Test>]
    let ``Linewise put at EOF places caret at start of line``() =
        assertColemakText "\n $  123" "jj;" "\n   123\n   1$23"

    [<Test>]
    let ``Multi line put places caret at top line of paste``() =
        assertColemakText "aa$a\nbbb\nccc\n" "Vns;" "ccc\na$aa\nbbb\n"

    [<Test>]
    let ``Multi line put on line without delimiter places caret at top line of paste``() =
        assertColemakText "aa$a\nbbb\nccc" "Vns;" "ccc\na$aa\nbbb"

    [<Test>]
    let ``x with multiplier stops at EOL``() =
        assertColemakText "ab$cdef\n" "100x;" "abcdef$\n"

    [<Test>]
    let ``2dw yank two words``() =
        assertColemakText "p$ublic int someInt = 1;" "2sw:" "public int s$omeInt = 1;"

    [<Test>]
    let ``2d} yank two paragraphs``() =
        let _  = testColemak " $\na\n\nb\n" "2s}"
        Vim.registers.[EmptyRegister].content |> should equal " \na\n\nb\n"

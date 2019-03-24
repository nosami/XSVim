namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Visual tests Dvorak`` =
    let getClipboard() = Vim.registers.[EmptyRegister].content

    [<Test>]
    let ``Visual to end of line``() =
        let _ = testDvorak "abc$ def\nghi" "k$f"
        getClipboard() |> should equal "c def"

    [<Test>]
    let ``Visual to end of word``() =
        let _ = testDvorak "ab$c def\nghi" "k.f"
        getClipboard() |> should equal "bc"

    [<Test>]
    let ``Visual to d``() =
        let _ = testDvorak "ab$cdef" "kydf"
        getClipboard() |> should equal "bc"

    [<Test>]
    let ``Visual to d inclusive``() =
        let _ = testDvorak "ab$cdef" "kudf"
        getClipboard() |> should equal "bcd"

    [<Test>]
    let ``Visual supports multipler``() =
        let _ = testDvorak "a$bcdef" "3kfy"
        getClipboard() |> should equal "abc"

    [<Test>]
    let ``Visual line``() =
        let _ = testDvorak "aaa\nbb$b\nddd" "Kf"
        getClipboard() |> should equal "bbb\n"

    [<Test>]
    let ``Visual to end of document``() =
        let _ = testDvorak "abc\nde$f\nghi" "kIf"
        getClipboard() |> should equal "ef\ng"

    [<Test>]
    let ``Visual to start of document``() =
        let _ = testDvorak "abc\nde$f\nghi" "kiif"
        getClipboard() |> should equal "abc\nde"

    [<Test>]
    let ``Visual line to end of document``() =
        let _ = testDvorak "abc\nde$f\nghi" "KIf"
        getClipboard() |> should equal "def\nghi"

    [<Test>]
    let ``Visual line to start of document``() =
        let _ = testDvorak "abc\nde$f\nghi" "Kiif"
        getClipboard() |> should equal "abc\ndef\n"

    [<Test>]
    let ``Visual line supports multipler``() =
        let _ = testDvorak "abc\nde$f\nghi" "2Kf"
        getClipboard() |> should equal "def\nghi"

    [<Test>]
    let ``Goto visual goes to last selection``() =
        let _ = testDvorak "abc\nde$f\nghi" "2K<esc>1Iikf"
        getClipboard() |> should equal "def\nghi"

    [<Test>]
    let ``Visual inside quotes``() =
        let _ = testDvorak "let s = \"some$ string\"" "kc_f"
        getClipboard() |> should equal "some string"

    [<Test>]
    let ``v]) goes to next unmatched )``() =
        let _ = testDvorak "if (a$ == (b)c)" "k=)f" 
        getClipboard() |> should equal "a == (b)c)"

    [<Test>]
    let ``caret moves to other end of selection``() =
        assertDvorakText "abc$def" "knnnr" "abc$def"

    [<Test>]
    let ``selection is not affected when you move to other end``() =
        let _ = testDvorak "abc$def" "knnnrf"
        getClipboard() |> should equal "cdef"

    [<Test>]
    let ``Moving to the EOF in visual mode does select text``() =
        let _  = testDvorak " $\na\n" "kIf"
        Vim.registers.[EmptyRegister].content |> should equal " \na\n"

    [<Test>]
    let ``Moving by a paragraph to the start of file does select text``() =
        let _ = testDvorak "start\n $" "k?f"
        Vim.registers.[EmptyRegister].content |> should equal "start\n "

namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Visual tests Colemak`` =
    let getClipboard() = Vim.registers.[EmptyRegister].content

    [<Test>]
    let ``Visual to end of line``() =
        let _ = testColemak "abc$ def\nghi" "v$j"
        getClipboard() |> should equal "c def"

    [<Test>]
    let ``Visual to end of word``() =
        let _ = testColemak "ab$c def\nghi" "vfj"
        getClipboard() |> should equal "bc"

    [<Test>]
    let ``Visual to d``() =
        let _ = testColemak "ab$cdef" "vgdj"
        getClipboard() |> should equal "bc"

    [<Test>]
    let ``Visual to d inclusive``() =
        let _ = testColemak "ab$cdef" "vtdj"
        getClipboard() |> should equal "bcd"

    [<Test>]
    let ``Visual supports multipler``() =
        let _ = testColemak "a$bcdef" "3vj"
        getClipboard() |> should equal "abc"

    [<Test>]
    let ``Visual line``() =
        let _ = testColemak "aaa\nbb$b\nddd" "Vj"
        getClipboard() |> should equal "bbb\n"

    [<Test>]
    let ``Visual to end of document``() =
        let _ = testColemak "abc\nde$f\nghi" "vDj"
        getClipboard() |> should equal "ef\ng"

    [<Test>]
    let ``Visual to start of document``() =
        let _ = testColemak "abc\nde$f\nghi" "vddj"
        getClipboard() |> should equal "abc\nde"

    [<Test>]
    let ``Visual line to end of document``() =
        let _ = testColemak "abc\nde$f\nghi" "VDj"
        getClipboard() |> should equal "def\nghi"

    [<Test>]
    let ``Visual line to start of document``() =
        let _ = testColemak "abc\nde$f\nghi" "Vddj"
        getClipboard() |> should equal "abc\ndef\n"

    [<Test>]
    let ``Visual line supports multipler``() =
        let _ = testColemak "abc\nde$f\nghi" "2Vj"
        getClipboard() |> should equal "def\nghi"

    [<Test>]
    let ``Goto visual goes to last selection``() =
        let _ = testColemak "abc\nde$f\nghi" "2V<esc>1Ddvj"
        getClipboard() |> should equal "def\nghi"

    [<Test>]
    let ``Visual inside quotes``() =
        let _ = testColemak "let s = \"some$ string\"" "vu\"j"
        getClipboard() |> should equal "some string"

    [<Test>]
    let ``v]) goes to next unmatched )``() =
        let _ = testColemak "if (a$ == (b)c)" "v])j" 
        getClipboard() |> should equal "a == (b)c)"

    [<Test>]
    let ``caret moves to other end of selection``() =
        assertColemakText "abc$def" "viiiy" "abc$def"

    [<Test>]
    let ``selection is not affected when you move to other end``() =
        let _ = testColemak "abc$def" "viiiyj"
        getClipboard() |> should equal "cdef"

    [<Test>]
    let ``Moving to the EOF in visual mode does select text``() =
        let _  = testColemak " $\na\n" "vDj"
        Vim.registers.[EmptyRegister].content |> should equal " \na\n"

    [<Test>]
    let ``Moving by a paragraph to the start of file does select text``() =
        let _ = testColemak "start\n $" "v{j"
        Vim.registers.[EmptyRegister].content |> should equal "start\n "

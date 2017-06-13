namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Visual tests`` =
    let getClipboard() = Vim.registers.[EmptyRegister].content

    [<Test>]
    let ``Visual to end of line``() =
        let _, state = test "abc$ def\nghi" "v$y"
        getClipboard() |> should equal "c def"

    [<Test>]
    let ``Visual to end of word``() =
        let _, state = test "ab$c def\nghi" "vey"
        getClipboard() |> should equal "bc"

    [<Test>]
    let ``Visual to d``() =
        let _, state = test "ab$cdef" "vtdy"
        getClipboard() |> should equal "bc"

    [<Test>]
    let ``Visual to d inclusive``() =
        let _, state = test "ab$cdef" "vfdy"
        getClipboard() |> should equal "bcd"

    [<Test>]
    let ``Visual supports multipler``() =
        let _, state = test "a$bcdef" "3vy"
        getClipboard() |> should equal "abc"

    [<Test>]
    let ``Visual line``() =
        let _, state = test "aaa\nbb$b\nddd" "Vy"
        getClipboard() |> should equal "bbb\n"

    [<Test>]
    let ``Visual to end of document``() =
        let _, state = test "abc\nde$f\nghi" "vGy"
        getClipboard() |> should equal "ef\ng"

    [<Test>]
    let ``Visual to start of document``() =
        let _, state = test "abc\nde$f\nghi" "vggy" 
        getClipboard() |> should equal "abc\nde"

    [<Test>]
    let ``Visual line to end of document``() =
        let _, state = test "abc\nde$f\nghi" "VGy"
        getClipboard() |> should equal "def\nghi"

    [<Test>]
    let ``Visual line to start of document``() =
        let _, state = test "abc\nde$f\nghi" "Vggy" 
        getClipboard() |> should equal "abc\ndef\n"

    [<Test>]
    let ``Visual line supports multipler``() =
        let _, state = test "abc\nde$f\nghi" "2Vy" 
        getClipboard() |> should equal "def\nghi"

    [<Test>]
    let ``Goto visual goes to last selection``() =
        let _, state = test "abc\nde$f\nghi" "2V<esc>1Ggvy"
        getClipboard() |> should equal "def\nghi"

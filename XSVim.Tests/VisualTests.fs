namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Visual tests`` =
    [<Test>]
    let ``Visual to end of line``() =
        let _, state = test "abc$ def\nghi" "v$y"
        Vim.registers.['+'] |> should equal "c def"

    [<Test>]
    let ``Visual to end of word``() =
        let _, state = test "ab$c def\nghi" "vey"
        Vim.registers.['+'] |> should equal "bc"

    [<Test>]
    let ``Visual to d``() =
        let _, state = test "ab$cdef" "vtdy"
        Vim.registers.['+'] |> should equal "bc"

    [<Test>]
    let ``Visual to d inclusive``() =
        let _, state = test "ab$cdef" "vfdy"
        Vim.registers.['+'] |> should equal "bcd"

    [<Test>]
    let ``Visual line``() =
        let _, state = test "aaa\nbb$b\nddd" "Vy"
        Vim.registers.['+'] |> should equal "bbb\n"

    [<Test>]
    let ``Visual to end of document``() =
        let _, state = test "abc\nde$f\nghi" "vGy"
        Vim.registers.['+'] |> should equal "ef\ng"

    [<Test>]
    let ``Visual to start of document``() =
        let _, state = test "abc\nde$f\nghi" "vggy" 
        Vim.registers.['+'] |> should equal "abc\nde"

    [<Test>]
    let ``Visual line to end of document``() =
        let _, state = test "abc\nde$f\nghi" "VGy"
        Vim.registers.['+'] |> should equal "def\nghi"

    [<Test>]
    let ``Visual line to start of document``() =
        let _, state = test "abc\nde$f\nghi" "Vggy" 
        Vim.registers.['+'] |> should equal "abc\ndef\n"
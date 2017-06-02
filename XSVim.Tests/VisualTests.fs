namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Visual tests`` =
    [<Test>]
    let ``Visual to end of line``() =
        let _, state = test "abc$ def\nghi" "v$y"
        Vim.clipboard |> should equal "c def"

    [<Test>]
    let ``Visual to end of word``() =
        let _, state = test "ab$c def\nghi" "vey"
        Vim.clipboard |> should equal "bc"

    [<Test>]
    let ``Visual to d``() =
        let _, state = test "ab$cdef" "vtdy"
        Vim.clipboard |> should equal "bc"

    [<Test>]
    let ``Visual to d inclusive``() =
        let _, state = test "ab$cdef" "vfdy"
        Vim.clipboard |> should equal "bcd"

    [<Test>]
    let ``Visual supports multipler``() =
        let _, state = test "a$bcdef" "3vy" 
        Vim.clipboard |> should equal "abc"       

    [<Test>]
    let ``Visual line``() =
        let _, state = test "aaa\nbb$b\nddd" "Vy"
        Vim.clipboard |> should equal "bbb\n"

    [<Test>]
    let ``Visual to end of document``() =
        let _, state = test "abc\nde$f\nghi" "vGy"
        Vim.clipboard |> should equal "ef\ng"

    [<Test>]
    let ``Visual to start of document``() =
        let _, state = test "abc\nde$f\nghi" "vggy" 
        Vim.clipboard |> should equal "abc\nde"

    [<Test>]
    let ``Visual line to end of document``() =
        let _, state = test "abc\nde$f\nghi" "VGy"
        Vim.clipboard |> should equal "def\nghi"

    [<Test>]
    let ``Visual line to start of document``() =
        let _, state = test "abc\nde$f\nghi" "Vggy" 
        Vim.clipboard |> should equal "abc\ndef\n"

    [<Test>]
    let ``Visual line supports multipler``() =
        let _, state = test "abc\nde$f\nghi" "2Vy" 
        Vim.clipboard |> should equal "def\nghi"       
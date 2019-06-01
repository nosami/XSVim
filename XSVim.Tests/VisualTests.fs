namespace XSVim.Tests
open NUnit.Framework
open XSVim
open System.Runtime.CompilerServices
open System.Threading.Tasks

[<TestFixture>]
module ``Visual tests`` =
    [<SetUp;AsyncStateMachine(typeof<Task>)>]
    let ``run before tests``() =
        FixtureSetup.initialiseMonoDevelop()

    let getClipboard() = Vim.registers.[EmptyRegister].content

    [<Test>]
    let ``Visual to end of line``() =
        let _ = test "abc$ def\nghi" "v$y"
        getClipboard() |> should equal "c def"

    [<Test>]
    let ``Visual to end of word``() =
        let _ = test "ab$c def\nghi" "vey"
        getClipboard() |> should equal "bc"

    [<Test>]
    let ``Visual to d``() =
        let _ = test "ab$cdef" "vtdy"
        getClipboard() |> should equal "bc"

    [<Test>]
    let ``Visual to d inclusive``() =
        let _ = test "ab$cdef" "vfdy"
        getClipboard() |> should equal "bcd"

    [<Test>]
    let ``Visual supports multipler``() =
        let _ = test "a$bcdef" "3vy"
        getClipboard() |> should equal "abc"

    [<Test>]
    let ``Visual line``() =
        let _ = test "aaa\nbb$b\nddd" "Vy"
        getClipboard() |> should equal "bbb\n"

    [<Test>]
    let ``Visual to end of document``() =
        let _ = test "abc\nde$f\nghi" "vGy"
        getClipboard() |> should equal "ef\ng"

    [<Test>]
    let ``Visual to start of document``() =
        let _ = test "abc\nde$f\nghi" "vggy"
        getClipboard() |> should equal "abc\nde"

    [<Test>]
    let ``Visual line to end of document``() =
        let _ = test "abc\nde$f\nghi" "VGy"
        getClipboard() |> should equal "def\nghi"

    [<Test>]
    let ``Visual line to start of document``() =
        let _ = test "abc\nde$f\nghi" "Vggy"
        getClipboard() |> should equal "abc\ndef\n"

    [<Test>]
    let ``Visual line supports multipler``() =
        let _ = test "abc\nde$f\nghi" "2Vy"
        getClipboard() |> should equal "def\nghi"

    [<Test>]
    let ``Goto visual goes to last selection``() =
        let _ = test "abc\nde$f\nghi" "2V<esc>1Ggvy"
        getClipboard() |> should equal "def\nghi"

    [<Test>]
    let ``Visual inside quotes``() =
        let _ = test "let s = \"some$ string\"" "vi\"y"
        getClipboard() |> should equal "some string"

    [<Test>]
    let ``v]) goes to next unmatched )``() =
        let _ = test "if (a$ == (b)c)" "v])y" 
        getClipboard() |> should equal "a == (b)c)"

    [<Test>]
    let ``caret moves to other end of selection``() =
        assertText "abc$def" "vlllo" "abc$def"

    [<Test>]
    let ``selection is not affected when you move to other end``() =
        let _ = test "abc$def" "vllloy"
        getClipboard() |> should equal "cdef"

    [<Test>]
    let ``Moving to the EOF in visual mode does select text``() =
        let _  = test " $\na\n" "vGy"
        Vim.registers.[EmptyRegister].content |> should equal " \na\n"

    [<Test>]
    let ``Moving by a paragraph to the start of file does select text``() =
        let _ = test "start\n $" "v{y"
        Vim.registers.[EmptyRegister].content |> should equal "start\n "

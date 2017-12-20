namespace XSVim.Tests
open NUnit.Framework
open XSVim
open MonoDevelop.Ide.Editor

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

    [<Test>]
    let ``Visual inside quotes``() =
        let _, state = test "let s = \"some$ string\"" "vi\"y"
        getClipboard() |> should equal "some string"

    [<Test>]
    let ``Selection outside vim``() =
        let editor = createEditor "a$bcdef"
        // simulate a selection using the mouse, or shift + arrow keys
        editor.SetSelection(0,3)
        editor.SelectedText |> should equal "abc"
        let state = Vim.processSelection editor VimState.Default
        state.mode |> should equal VisualMode
        let newState = processKeys editor "y" state
        getClipboard() |> should equal "abc"

    [<Test>]
    let ``Left to right selection outside vim can be extended``() =
        let editor = createEditor "a$bcdef"
        // simulate a selection using the mouse, or shift + arrow keys
        editor.SetSelection(0,3) // a -> c
        editor.SelectedText |> should equal "abc"
        let state = Vim.processSelection editor VimState.Default
        let newState = processKeys editor "l" state
        editor.SelectedText |> should equal "abcd"

    [<Test>]
    let ``Left to right selection outside vim can be contracted``() =
        let editor = createEditor "a$bcdef"
        // simulate a selection using the mouse, or shift + arrow keys
        editor.SetSelection(0,3) // a -> c
        editor.SelectedText |> should equal "abc"
        let state = Vim.processSelection editor VimState.Default
        let newState = processKeys editor "hy" state
        getClipboard() |> should equal "ab"

    [<Test>]
    let ``Right to left selection inside vim can be contracted``() =
        let editor = createEditor " abc$def"
        // simulate a selection using the mouse, or shift + arrow keys
        //editor.SetSelection(4,1) // c -> a
        let state = processKeys editor "vhh" VimState.Default
        //editor.SelectedText |> should equal "abc"
        let state = Vim.processSelection editor state
        let newState = processKeys editor "ly" state
        getClipboard() |> should equal "bc"

    [<Test>]
    let ``Right to left selection outside vim can be contracted``() =
        let editor = createEditor " a$bcdef"
        // simulate a selection using the mouse, or shift + arrow keys
        editor.SetSelection(4,1) // c -> a
        editor.SelectedText |> should equal "abc"
        let state = Vim.processSelection editor VimState.Default
        let newState = processKeys editor "ly" state
        getClipboard() |> should equal "bc"

    [<Test>]
    let ``Right to left selection outside vim can be expanded``() =
        let editor = createEditor "a$bcdef"
        // simulate a selection using the mouse, or shift + arrow keys
        editor.SetSelection(4,1) // d -> b
        editor.SelectedText |> should equal "bcd"
        let state = Vim.processSelection editor VimState.Default
        let newState = processKeys editor "hy" state
        getClipboard() |> should equal "abcd"

    [<Test>]
    let ``Visual line mode is retained``() =
        let editor = createEditor "a$bcdef"
        let state = processKeys editor "V" VimState.Default
        state.mode |> should equal VisualLineMode
        let state = Vim.processSelection editor state
        state.mode |> should equal VisualLineMode

    [<Test>]
    let ``Clearing selection reverts to normal mode``() =
        let editor = createEditor "a$bcdef"
        let state = processKeys editor "V" VimState.Default
        state.mode |> should equal VisualLineMode
        editor.ClearSelection()
        let state = Vim.processSelection editor state
        state.mode |> should equal NormalMode

    [<Test>]
    let ``Process selection doesn't modify selection``() =
        let editor = createEditor "a$bcdef"
        let state1 = processKeys editor "vl" VimState.Default
        let state2 = Vim.processSelection editor state1
        let state3 = Vim.processSelection editor state2
        let state4 = processKeys editor "y" state3
        getClipboard() |> should equal "ab"
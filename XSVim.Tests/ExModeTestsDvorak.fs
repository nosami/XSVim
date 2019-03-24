namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Ex mode tests Dvorak`` =
    [<Test>]
    let ``/ searches for word``() =
        assertDvorakText "ab$c abc" "zabc<ret>" "abc a$bc"

    [<Test>]
    let ``/ is case insensitive``() =
        assertDvorakText "ab$c ABC" "zabc<ret>" "abc A$BC"

    [<Test>]
    let ``/ is case sensitive``() =
        assertDvorakText "ab$c ABC Abc" "zAbc<ret>" "abc ABC A$bc"

    [<Test>]
    let ``deletes to search term``() =
        assertDvorakText "ab$c ABC Abc 123" "ez123<ret>" "a1$23"

    [<Test>]
    let ``n searches for next word``() =
        assertDvorakText "ab$c abc abc" "zabc<ret>b" "abc abc a$bc"

    [<Test>]
    let ``n wraps to start``() =
        assertDvorakText "ab$c abc abc" "zabc<ret>bb" "a$bc abc abc"

    [<Test>]
    let ``N searches for previous word``() =
        assertDvorakText "ab$c abc abc" "zabc<ret>B" "a$bc abc abc"

    [<Test>]
    let ``n searches for previous word after ?``() =
        assertDvorakText "abc abc a$bc" "Zabc<ret>b" "a$bc abc abc"

    [<Test>]
    let ``? searches for word backwards``() =
        assertDvorakText "abc abc a$bc" "Zabc<ret>" "abc a$bc abc"

    [<Test>]
    let ``:2 jumps to line 2``() =
        assertDvorakText "l$ine1\nline2" "S2<ret>" "line1\nl$ine2"

    [<Test>]
    let ``Backspacing ex mode returns to normal mode``() =
        let _, state, _ = testDvorak "abc abc a$bc" "za<bs><bs>"
        state.mode |> should equal NormalMode

    [<Test>]
    let ``<esc> returns to normal mode``() =
        let _, state, _ = testDvorak "abc abc a$bc" "z<esc>"
        state.mode |> should equal NormalMode

    [<Test>]
    let ``<C-c> returns to normal mode``() =
        let _, state, _ = testDvorak "abc abc a$bc" "z<C-c>"
        state.mode |> should equal NormalMode

    [<Test>]
    let ``<C-[> returns to normal mode``() =
        let _, state, _ = testDvorak "abc abc a$bc" "z<C-[>"
        state.mode |> should equal NormalMode

    [<Test>]
    let ``Displays could not parse message``() =
        let _, state, _ = testDvorak "a$bc" "Sgarbage<ret>"
        state.statusMessage.Value |> should equal "Could not parse :garbage"

    [<Test>]
    let ``Could not parse message is reset``() =
        let _, state, _ = testDvorak "a$bc" "Sgarbage<ret>n"
        state.statusMessage |> should equal None

    [<Test>]
    let ``Deletes lines 2 to 4``() =
        assertDvorakText
            """11111
22222
33333
44444
55555$"""

            "S2,4d<ret>"

            """11111
5$5555"""

    [<Test>]
    let ``Switching to substitute command mode with a selection``() =
        let _, state, _ = testDvorak "a$bc" "kS"
        state.statusMessage |> should equal (Some ":'<,'>")

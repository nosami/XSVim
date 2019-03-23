namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Ex mode tests Colemak`` =
    [<Test>]
    let ``/ searches for word``() =
        assertColemakText "ab$c abc" "/abc<ret>" "abc a$bc"

    [<Test>]
    let ``/ is case insensitive``() =
        assertColemakText "ab$c ABC" "/abc<ret>" "abc A$BC"

    [<Test>]
    let ``/ is case sensitive``() =
        assertColemakText "ab$c ABC Abc" "/Abc<ret>" "abc ABC A$bc"

    [<Test>]
    let ``deletes to search term``() =
        assertColemakText "ab$c ABC Abc 123" "s/123<ret>" "a1$23"

    [<Test>]
    let ``n searches for next word``() =
        assertColemakText "ab$c abc abc" "/abc<ret>k" "abc abc a$bc"

    [<Test>]
    let ``n wraps to start``() =
        assertColemakText "ab$c abc abc" "/abc<ret>kk" "a$bc abc abc"

    [<Test>]
    let ``N searches for previous word``() =
        assertColemakText "ab$c abc abc" "/abc<ret>K" "a$bc abc abc"

    [<Test>]
    let ``n searches for previous word after ?``() =
        assertColemakText "abc abc a$bc" "?abc<ret>k" "a$bc abc abc"

    [<Test>]
    let ``? searches for word backwards``() =
        assertColemakText "abc abc a$bc" "?abc<ret>" "abc a$bc abc"

    [<Test>]
    let ``:2 jumps to line 2``() =
        assertColemakText "l$ine1\nline2" "O2<ret>" "line1\nl$ine2"

    [<Test>]
    let ``Backspacing ex mode returns to normal mode``() =
        let _, state, _ = testColemak "abc abc a$bc" "/a<bs><bs>"
        state.mode |> should equal NormalMode

    [<Test>]
    let ``<esc> returns to normal mode``() =
        let _, state, _ = testColemak "abc abc a$bc" "/<esc>"
        state.mode |> should equal NormalMode

    [<Test>]
    let ``<C-c> returns to normal mode``() =
        let _, state, _ = testColemak "abc abc a$bc" "/<C-c>"
        state.mode |> should equal NormalMode

    [<Test>]
    let ``<C-[> returns to normal mode``() =
        let _, state, _ = testColemak "abc abc a$bc" "/<C-[>"
        state.mode |> should equal NormalMode

    [<Test>]
    let ``Displays could not parse message``() =
        let _, state, _ = testColemak "a$bc" "Ogarbage<ret>"
        state.statusMessage.Value |> should equal "Could not parse :garbage"

    [<Test>]
    let ``Could not parse message is reset``() =
        let _, state, _ = testColemak "a$bc" "Ogarbage<ret>i"
        state.statusMessage |> should equal None

    [<Test>]
    let ``Deletes lines 2 to 4``() =
        assertColemakText
            """11111
22222
33333
44444
55555$"""

            "O2,4d<ret>"

            """11111
5$5555"""

    [<Test>]
    let ``Switching to substitute command mode with a selection``() =
        let _, state, _ = testColemak "a$bc" "vO"
        state.statusMessage |> should equal (Some ":'<,'>")

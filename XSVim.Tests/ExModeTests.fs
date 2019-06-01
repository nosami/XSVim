namespace XSVim.Tests
open NUnit.Framework
open XSVim
open System.Runtime.CompilerServices
open System.Threading.Tasks

[<TestFixture>]
module ``Ex mode tests`` =
    [<SetUp;AsyncStateMachine(typeof<Task>)>]
    let ``run before tests``() =
        FixtureSetup.initialiseMonoDevelop()

    [<Test>]
    let ``/ searches for word``() =
        assertText "ab$c abc" "/abc<ret>" "abc a$bc"

    [<Test>]
    let ``/ is case insensitive``() =
        //assertDvorakText "ab$c ABC" "zabc<ret>" "abc A$BC"
        assertText "ab$c ABC" "/abc<ret>" "abc A$BC"

    [<Test>]
    let ``/ is case sensitive``() =
        assertText "ab$c ABC Abc" "/Abc<ret>" "abc ABC A$bc"

    [<Test>]
    let ``deletes to search term``() =
        assertText "ab$c ABC Abc 123" "d/123<ret>" "a1$23"

    [<Test>]
    let ``n searches for next word``() =
        assertText "ab$c abc abc" "/abc<ret>n" "abc abc a$bc"

    [<Test>]
    let ``n wraps to start``() =
        assertText "ab$c abc abc" "/abc<ret>nn" "a$bc abc abc"

    [<Test>]
    let ``N searches for previous word``() =
        assertText "ab$c abc abc" "/abc<ret>N" "a$bc abc abc"

    [<Test>]
    let ``n searches for previous word after ?``() =
        assertText "abc abc a$bc" "?abc<ret>n" "a$bc abc abc"

    [<Test>]
    let ``? searches for word backwards``() =
        assertText "abc abc a$bc" "?abc<ret>" "abc a$bc abc"

    [<Test>]
    let ``:2 jumps to line 2``() =
        assertText "l$ine1\nline2" ":2<ret>" "line1\nl$ine2"

    [<Test>]
    let ``Backspacing ex mode returns to normal mode``() =
        let _, state, _ = test "abc abc a$bc" "/a<bs><bs>"
        state.mode |> should equal NormalMode

    [<Test>]
    let ``<esc> returns to normal mode``() =
        let _, state, _ = test "abc abc a$bc" "/<esc>"
        state.mode |> should equal NormalMode

    [<Test>]
    let ``<C-c> returns to normal mode``() =
        let _, state, _ = test "abc abc a$bc" "/<C-c>"
        state.mode |> should equal NormalMode

    [<Test>]
    let ``<C-[> returns to normal mode``() =
        let _, state, _ = test "abc abc a$bc" "/<C-[>"
        state.mode |> should equal NormalMode

    [<Test>]
    let ``Displays could not parse message``() =
        let _, state, _ = test "a$bc" ":garbage<ret>"
        state.statusMessage.Value |> should equal "Could not parse :garbage"

    [<Test>]
    let ``Could not parse message is reset``() =
        let _, state, _ = test "a$bc" ":garbage<ret>l"
        state.statusMessage |> should equal None

    [<Test>]
    let ``Deletes lines 2 to 4``() =
        assertText
            """11111
22222
33333
44444
55555$"""

            ":2,4d<ret>"

            """11111
5$5555"""

    [<Test>]
    let ``Switching to substitute command mode with a selection``() =
        let _, state, _ = test "a$bc" "v:"
        state.statusMessage |> should equal (Some ":'<,'>")

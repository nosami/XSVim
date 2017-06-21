namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Ex mode tests`` =
    [<Test>]
    let ``/ searches for word``() =
        assertText "ab$c abc" "/abc<ret>" "abc a$bc"

    [<Test>]
    let ``n searches for next word``() =
        assertText "ab$c abc abc" "/abc<ret>n" "abc abc a$bc"

    [<Test>]
    let ``n wraps to start``() =
        assertText "ab$c abc abc" "/abc<ret>nn" "a$bc abc abc"

    [<Test>]
    let ``? searches for word backwards``() =
        assertText "abc abc a$bc" "?abc<ret>" "abc a$bc abc"

    [<Test>]
    let ``Backspacing ex mode returns to normal mode``() =
        let _, state = test "abc abc a$bc" "/a<bs><bs>"
        state.mode |> should equal NormalMode
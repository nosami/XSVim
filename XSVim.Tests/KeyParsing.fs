namespace XSVim.Tests
open XSVim
open NUnit.Framework

[<TestFixture>]
module ``Key parsing tests`` =
    let test keys =
        let keys = [for c in keys -> c.ToString()]
        let state = { keys=keys; mode=NormalMode; visualStartOffset=0; findCharCommand=None; lastAction=[]; clipboard=""; desiredColumn=0 }
        let action, _state = Vim.parseKeys state
        let first = action.Head
        first.repeat, first.commandType, first.textObject

    [<Test>]
    let ``10j``() =
        test "10j" |> should equal (10, Move, Down)

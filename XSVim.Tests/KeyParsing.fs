namespace XSVim.Tests
open XSVim
open NUnit.Framework

[<TestFixture>]
module ``Key parsing tests`` =
    let test keys =
        let keys = [for c in keys -> c.ToString()]
        let state = { Vim.defaultState with keys=keys }
        let action, _state = Vim.parseKeys state
        let first = action.Head

        first.repeat, first.commandType, first.textObject

    [<Test>]
    let ``10j``() =
        test "10j" |> should equal (Some 10, Move, Down)

    [<Test>]
    let ``11G``() =
        test "11G" |> should equal (Some 1, Move, StartOfLineNumber 11)

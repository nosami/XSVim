namespace XSVim.Tests
open XSVim
open NUnit.Framework

[<TestFixture>]
module ``Key parsing tests`` =
    let test keys =
        let keys = [for c in keys -> Key c]
        let state = { VimState.Default with keys=keys }
        let config = { insertModeEscapeKey = None }
        let action, _state = Vim.parseKeys state config
        let first = action.Head

        first.repeat, first.commandType, first.textObject

    [<Test>]
    let ``10j``() =
        test "10j" |> should equal (Some 10, Move, Down)

    [<Test>]
    let ``11G``() =
        test "11G" |> should equal (Some 1, Move, Jump (StartOfLineNumber 11))

namespace XSVim.Tests
open XSVim
open NUnit.Framework

[<TestFixture>]
module ``Key parsing tests Colemak`` =
    let test keys =
        let keys = [for c in keys -> Key c]
        let state = { VimState.Default with keys=keys }
        let config = { Config.Default with keyboardLayout = Colemak }
        let action, _state = Vim.parseKeys state config
        let first = action.Head

        first.repeat, first.commandType, first.textObject

    [<Test>]
    let ``10j``() =
        test "10n" |> should equal (Some 10, Move, Down)

    [<Test>]
    let ``11G``() =
        test "11D" |> should equal (Some 1, Move, Jump (StartOfLineNumber 11))

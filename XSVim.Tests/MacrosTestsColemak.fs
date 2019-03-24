namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Macro tests Colemak`` =
    [<Test>]
    let ``Start recording macro q``() =
        let _, state, _ = testColemak " $" "qq"
        state.macro |> should equal (Some (Macro 'q'))

    [<Test>]
    let ``Stop recording macro q``() =
        let _, state, _ = testColemak " $" "qqq"
        state.macro |> should equal None

    [<Test>]
    let ``Replay macro q``() =
        assertColemakText "a$bc abc" "qqtcad<esc>q@q" "abcd abcd$"

    [<Test>]
    let ``Macros are repeatable``() =
        assertColemakText "a$bc abc abc abc" "qqtcad<esc>q3@q" "abcd abcd abcd abcd$"

    [<Test>]
    let ``Macros containing repeats are repeatable``() =
        assertColemakText " $aa aa aa aa" "qq2taab<esc>q3@q" " aab aab aab aab$"


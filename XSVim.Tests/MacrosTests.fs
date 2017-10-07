namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Macro tests`` =
    [<Test>]
    let ``Start recording macro q``() =
        let _, state, _ = test " $" "qq"
        state.macro |> should equal (Some (Macro 'q'))

    [<Test>]
    let ``Stop recording macro q``() =
        let _, state, _ = test " $" "qqq"
        state.macro |> should equal None

    [<Test>]
    let ``Replay macro q``() =
        assertText "a$bc abc" "qqfcad<esc>q@q" "abcd abcd$"

    [<Test>]
    let ``Macros are repeatable``() =
        assertText "a$bc abc abc abc" "qqfcad<esc>q3@q" "abcd abcd abcd abcd$"

    [<Test>]
    let ``Macros containing repeats are repeatable``() =
        assertText " $aa aa aa aa" "qq2faab<esc>q3@q" " aab aab aab aab$"


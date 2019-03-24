namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Macro tests Dvorak`` =
    [<Test>]
    let ``Start recording macro q``() =
        let _, state, _ = testDvorak " $" "'q"
        state.macro |> should equal (Some (Macro 'q'))

    [<Test>]
    let ``Stop recording macro q``() =
        let _, state, _ = testDvorak " $" "'q'"
        state.macro |> should equal None

    [<Test>]
    let ``Replay macro q``() =
        assertDvorakText "a$bc abc" "'qucad<esc>'@q" "abcd abcd$"

    [<Test>]
    let ``Macros are repeatable``() =
        assertDvorakText "a$bc abc abc abc" "'qucad<esc>'3@q" "abcd abcd abcd abcd$"

    [<Test>]
    let ``Macros containing repeats are repeatable``() =
        assertDvorakText " $aa aa aa aa" "'q2uaab<esc>'3@q" " aab aab aab aab$"


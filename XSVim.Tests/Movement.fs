namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Movement tests`` =
    [<Test>]
    let ``Move to next word``() =
        assertText "aa$a bbb" "w" "aaa b$bb"

    [<Test>]
    let ``Move to word end``() =
        assertText "aa$a bbb" "e" "aaa$ bbb"

    [<Test>]
    let ``Move to end of line``() =
        assertText "aa$a aaa\nbbb" "$" "aaa aaa$\nbbb"

    [<Test>]
    let ``Move to end of document``() =
        assertText "aa$aaaa\nbbbbbb" "G" "aaaaaa\nb$bbbbb"

    [<Test>]
    let ``Move to start of document``() =
        assertText "aaaaaa\nbb$bbbb" "gg" "a$aaaaa\nbbbbbb"
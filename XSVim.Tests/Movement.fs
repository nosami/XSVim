namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Movement tests`` =
    [<Test>]
    let ``Move to next word``() =
        test "aa$a bbb" "w" "aaa b$bb"

    [<Test>]
    let ``Move to word end``() =
        test "aa$a bbb" "e" "aaa$ bbb"

    [<Test>]
    let ``Move to end of line``() =
        test "aa$a aaa\nbbb" "$" "aaa aaa$\nbbb"

    [<Test>]
    let ``Move to end of document``() =
        test "aa$aaaa\nbbbbbb" "G" "aaaaaa\nb$bbbbb"

    [<Test>]
    let ``Move to start of document``() =
        test "aaaaaa\nbb$bbbb" "gg" "a$aaaaa\nbbbbbb"
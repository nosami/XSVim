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

    [<Test>]
    let ``Move down to desired column``() =
        assertText "12345$6\n123\n123456" "jj" "123456\n123\n12345$6"

    [<Test>]
    let ``Move ten right``() =
        assertText "a$bcdefghijkl" "10l" "abcdefghijk$l"
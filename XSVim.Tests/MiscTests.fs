namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Miscellaneous tests`` =
    [<Test>]
    let ``'A' should put caret at end of the line``() =
        assertText "abc$def\n" "A" "abcdef|\n"

    [<Test>]
    let ``'a' should append after``() =
        assertText "a$bcdef" "a" "a|bcdef"

    [<Test>]
    let ``'a' should append after last char``() =
        assertText "abcdef$\n" "a" "abcdef|\n"

    [<Test>]
    let ``'a' should append before EOF``() =
        assertText "abcdef$" "a" "abcdef|"

    [<Test>]
    let ``'a' on empty line should keep cursor on the current line``() =
        assertText "\n$abc" "a" "|\nabc"

    [<Test>]
    let ``'I' should insert at first non whitespace``() =
        assertText "   abcdef$" "I" "   |abcdef"
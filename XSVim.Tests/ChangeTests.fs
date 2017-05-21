namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Change tests`` =
    [<Test>]
    let ``cc non empty line``() =
        assertText "abc\nd$ef\nghi" "cc" "abc\n|\nghi"

    [<Test>]
    let ``cw changes word``() =
        assertText "a$bc def" "cw" "| def"

    [<Test>]
    let ``ce changes word``() =
        assertText "a$bc def" "ce" "| def"

    [<Test>]
    let ``cc empty line``() =
        assertText "abc\n\n$def" "cc" "abc\n|\ndef"
namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Change tests`` =
    [<Test>]
    let ``cc non empty line``() =
        assertText "abc\nd$ef\nghi" "cc" "abc\n|\nghi"

    [<Test>]
    let ``cc empty line``() =
        assertText "abc\n\n$def" "cc" "abc\n|\ndef"
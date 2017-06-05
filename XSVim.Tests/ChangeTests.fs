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
    let ``cw changes space``() =
        assertText "abc $def" "cw" "abc|def"

    [<Test>]
    let ``c2w changes two words``() =
        assertText "a$bc def ghi" "c2w" "| ghi"

    [<Test>]
    let ``undo works after cw``() =
        assertText "a$bc def ghi" "cw<esc>u" "a$bc def ghi"

    [<Test>]
    let ``undo works after c2w``() =
        assertText "a$bc def ghi" "c2w<esc>u" "a$bc def ghi"

    [<Test>]
    let ``ce changes word``() =
        assertText "a$bc def" "ce" "| def"

    [<Test>]
    let ``cc empty line``() =
        assertText "abc\n\n$def" "cc" "abc\n|\ndef"

    [<Test>]
    let ``ci backtick``() =
        assertText "``some t$ext``" "ci`" "``|``"
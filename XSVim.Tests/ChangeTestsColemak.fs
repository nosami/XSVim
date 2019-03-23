namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Change tests Colemak`` =
    [<Test>]
    let ``cc non empty line``() =
        assertColemakText "abc\nd$ef\nghi" "cc" "abc\n|\nghi"

    [<Test>]
    let ``cw changes word``() =
        assertColemakText "a$bc    def" "cw" "|    def"

    [<Test>]
    let ``C changes last character``() =
        assertColemakText "abc$" "C" "ab|"

    [<Test>]
    let ``cw changes space``() =
        assertColemakText "abc $def" "cw" "abc|def"

    [<Test>]
    let ``c2w changes two words``() =
        assertColemakText "a$bc def ghi" "c2w" "| ghi"

    [<Test>]
    let ``undo works after cw``() =
        assertColemakText "a$bc def ghi" "cw<esc>l" "a$bc def ghi"

    [<Test>]
    let ``undo works after c2w``() =
        assertColemakText "a$bc def ghi" "c2w<esc>l" "a$bc def ghi"

    [<Test>]
    let ``ce changes word``() =
        assertColemakText "a$bc def" "cf" "| def"

    [<Test>]
    let ``ce changes last character``() =
        assertColemakText "a$ bcd" "cf" "| bcd"

    [<Test>]
    let ``c% changes to matching parens``() =
        assertColemakText "abc($def)ghi" "c%" "abc|ghi"

    [<Test>]
    let ``Change to end of word does not include dot``() =
        assertColemakText "open Mon$o.Addins" "cf" "open Mo|.Addins"

    [<Test>]
    let ``Change to end of word includes dot``() =
        assertColemakText "open Mono$.Addins" "cf" "open Mon|Addins"

    [<Test>]
    let ``cc empty line``() =
        assertColemakText "abc\n\n$def" "cc" "abc\n|\ndef"

    [<Test>]
    let ``ci backtick``() =
        assertColemakText "``some t$ext``" "cu`" "``|``"

    [<Test>]
    let ``S changes entire line``() =
        assertColemakText " line1 \n line2$ \n line3 " "R" " line1 \n |\n line3 "

    [<Test>]
    let ``2S changes two lines``() =
        assertColemakText " line1 \n line2$ \n line3 \n line4 " "2R" " line1 \n |\n line4 "

    [<Test>]
    let ``s before the end of line``() =
        assertColemakText "a$b" "r" "|b"

    [<Test>]
    let ``s at the end of line``() =
        assertColemakText "ab$\n" "r" "a|\n"

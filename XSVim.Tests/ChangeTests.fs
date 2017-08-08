namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Change tests`` =
    [<Test>]
    let ``cc non empty line``() =
        assertText "abc\nd$ef\nghi" "cc" "abc\n|\nghi"

    [<Test>]
    let ``cw changes word``() =
        assertText "a$bc    def" "cw" "|    def"

    [<Test>]
    let ``C changes last character``() =
        assertText "abc$" "C" "ab|"

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
    let ``ce changes last character``() =
        assertText "a$ bcd" "ce" "| bcd"

    [<Test>]
    let ``Change to end of word does not include dot``() =
        assertText "open Mon$o.Addins" "ce" "open Mo|.Addins"

    [<Test>]
    let ``Change to end of word includes dot``() =
        assertText "open Mono$.Addins" "ce" "open Mon|Addins"

    [<Test>]
    let ``cc empty line``() =
        assertText "abc\n\n$def" "cc" "abc\n|\ndef"

    [<Test>]
    let ``ci backtick``() =
        assertText "``some t$ext``" "ci`" "``|``"

    [<Test>]
    let ``S changes entire line``() =
        assertText " line1 \n line2$ \n line3 " "S" " line1 \n|\n line3 "

    [<Test>]
    let ``2S changes two lines``() =
        assertText " line1 \n line2$ \n line3 \n line4 " "2S" " line1 \n|\n line4 "

    [<Test>]
    let ``s at the end of line``() =
        assertText "ab$\n" "s" "a|\n"

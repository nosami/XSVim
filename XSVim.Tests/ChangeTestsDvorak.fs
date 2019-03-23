namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Change tests Dvorak`` =
    [<Test>]
    let ``cc non empty line``() =
        assertDvorakText "abc\nd$ef\nghi" "jj" "abc\n|\nghi"

    [<Test>]
    let ``cw changes word``() =
        assertDvorakText "a$bc    def" "j," "|    def"

    [<Test>]
    let ``C changes last character``() =
        assertDvorakText "abc$" "J" "ab|"

    [<Test>]
    let ``cw changes space``() =
        assertDvorakText "abc $def" "j," "abc|def"

    [<Test>]
    let ``c2w changes two words``() =
        assertDvorakText "a$bc def ghi" "j2," "| ghi"

    [<Test>]
    let ``undo works after cw``() =
        assertDvorakText "a$bc def ghi" "j,<esc>g" "a$bc def ghi"

    [<Test>]
    let ``undo works after c2w``() =
        assertDvorakText "a$bc def ghi" "j2,<esc>g" "a$bc def ghi"

    [<Test>]
    let ``ce changes word``() =
        assertDvorakText "a$bc def" "j." "| def"

    [<Test>]
    let ``ce changes last character``() =
        assertDvorakText "a$ bcd" "j." "| bcd"

    [<Test>]
    let ``c% changes to matching parens``() =
        assertDvorakText "abc($def)ghi" "j%" "abc|ghi"

    [<Test>]
    let ``Change to end of word does not include dot``() =
        assertDvorakText "open Mon$o.Addins" "j." "open Mo|.Addins"

    [<Test>]
    let ``Change to end of word includes dot``() =
        assertDvorakText "open Mono$.Addins" "j." "open Mon|Addins"

    [<Test>]
    let ``cc empty line``() =
        assertDvorakText "abc\n\n$def" "jj" "abc\n|\ndef"

    [<Test>]
    let ``ci backtick``() =
        assertDvorakText "``some t$ext``" "jc`" "``|``"

    [<Test>]
    let ``S changes entire line``() =
        assertDvorakText " line1 \n line2$ \n line3 " "O" " line1 \n |\n line3 "

    [<Test>]
    let ``2S changes two lines``() =
        assertDvorakText " line1 \n line2$ \n line3 \n line4 " "2O" " line1 \n |\n line4 "

    [<Test>]
    let ``s before the end of line``() =
        assertDvorakText "a$b" "o" "|b"

    [<Test>]
    let ``s at the end of line``() =
        assertDvorakText "ab$\n" "o" "a|\n"

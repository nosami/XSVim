namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Indentation tests Dvorak`` =
    [<Test>]
    let ``>> indents right in normal mode``() =
        assertDvorakText "a$bc" "VV" "    a$bc"

    [<Test>]
    let ``indent is repeatable``() =
        assertDvorakText "a$bc" "VVv" "        a$bc"

    [<Test>]
    let ``V> indents line right``() =
        let text, state, _ = testDvorak "a$bc\ndef" "KV"
        text |> should equal "    a$bc\ndef"
        state.mode |> should equal NormalMode

    [<Test>]
    let ``V2> indents line right twice``() =
        assertDvorakText "a$bc\ndef" "K2V" "        a$bc\ndef"

    [<Test>]
    let ``>j indents current line and line below``() =
        assertDvorakText "a$bc\ndef" "Vh" "    a$bc\n    def"

    [<Test>]
    let ``<j unindents current line and line below``() =
        assertDvorakText "    a$bc\n    def" "Wh" "a$bc\ndef"

    [<Test;Ignore("Doesn't place caret at correct location")>]
    let ``>2j indents current line and two lines below``() =
        assertDvorakText "a$bc\ndef\nghi" "V2h" "    a$bc\n    def\n    ghi"

    [<Test>]
    let ``>gg indents to top of file``() =
        assertDvorakText "abc\ndef\ngh$i" "Vii" "    abc\n    def\n    gh$i"

    [<Test>]
    let ``V> indents line``() =
        assertDvorakText "abc\ndef\ngh$i" "Vii" "    abc\n    def\n    gh$i"

    [<Test>]
    let ``>2gg indents to line 2``() =
        assertDvorakText "abc\ndef\ngh$i" "V2ii" "abc\n    def\n    gh$i"

    [<Test>]
    let ``>2G indents to line 2``() =
        assertDvorakText "abc\ndef\ngh$i" "V2I" "abc\n    def\n    gh$i"

    [<Test>]
    let ``== autoindents line``() =
        assertDvorakText "abc\n    def\ngh$i" "]]" "abc\n    def\n    g$hi"

    [<Test>]
    let ``= autoindents selection``() =
        assertDvorakText "abc\n    def\ngh$i" "K]" "abc\n    def\n    g$hi"

    [<Test>]
    let ``= autoindents multiple line selection``() =
        assertDvorakText "abc\n    de$f\n   ghi\n   jkl" "Kh]" "abc\nd$ef\nghi\n   jkl"

    [<Test>]
    let ``=gg indents to top of file``() =
        assertDvorakText "abc\n  def\n  gh$i" "]ii" "a$bc\ndef\nghi"
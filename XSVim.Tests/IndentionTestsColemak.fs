namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Indentation tests Colemak`` =
    [<Test>]
    let ``>> indents right in normal mode``() =
        assertColemakText "a$bc" ">>" "    a$bc"

    [<Test>]
    let ``indent is repeatable``() =
        assertColemakText "a$bc" ">>." "        a$bc"

    [<Test>]
    let ``V> indents line right``() =
        let text, state, _ = testColemak "a$bc\ndef" "V>"
        text |> should equal "    a$bc\ndef"
        state.mode |> should equal NormalMode

    [<Test>]
    let ``V2> indents line right twice``() =
        assertColemakText "a$bc\ndef" "V2>" "        a$bc\ndef"

    [<Test>]
    let ``>j indents current line and line below``() =
        assertColemakText "a$bc\ndef" ">n" "    a$bc\n    def"

    [<Test>]
    let ``<j unindents current line and line below``() =
        assertColemakText "    a$bc\n    def" "<n" "a$bc\ndef"

    [<Test;Ignore("Doesn't place caret at correct location")>]
    let ``>2j indents current line and two lines below``() =
        assertColemakText "a$bc\ndef\nghi" ">2n" "    a$bc\n    def\n    ghi"

    [<Test>]
    let ``>gg indents to top of file``() =
        assertColemakText "abc\ndef\ngh$i" ">dd" "    abc\n    def\n    gh$i"

    [<Test>]
    let ``V> indents line``() =
        assertColemakText "abc\ndef\ngh$i" ">dd" "    abc\n    def\n    gh$i"

    [<Test>]
    let ``>2gg indents to line 2``() =
        assertColemakText "abc\ndef\ngh$i" ">2dd" "abc\n    def\n    gh$i"

    [<Test>]
    let ``>2G indents to line 2``() =
        assertColemakText "abc\ndef\ngh$i" ">2D" "abc\n    def\n    gh$i"

    [<Test>]
    let ``== autoindents line``() =
        assertColemakText "abc\n    def\ngh$i" "==" "abc\n    def\n    g$hi"

    [<Test>]
    let ``= autoindents selection``() =
        assertColemakText "abc\n    def\ngh$i" "V=" "abc\n    def\n    g$hi"

    [<Test>]
    let ``= autoindents multiple line selection``() =
        assertColemakText "abc\n    de$f\n   ghi\n   jkl" "Vn=" "abc\nd$ef\nghi\n   jkl"

    [<Test>]
    let ``=gg indents to top of file``() =
        assertColemakText "abc\n  def\n  gh$i" "=dd" "a$bc\ndef\nghi"

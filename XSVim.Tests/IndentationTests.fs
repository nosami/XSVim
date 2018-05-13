namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Indentation tests`` =
    [<Test>]
    let ``>> indents right in normal mode``() =
        assertText "a$bc" ">>" "    a$bc"

    [<Test>]
    let ``indent is repeatable``() =
        assertText "a$bc" ">>." "        a$bc"

    [<Test>]
    let ``V> indents line right``() =
        let text, state, _ = test "a$bc\ndef" "V>"
        text |> should equal "    a$bc\ndef"
        state.mode |> should equal NormalMode

    [<Test>]
    let ``V2> indents line right twice``() =
        assertText "a$bc\ndef" "V2>" "        a$bc\ndef"

    [<Test>]
    let ``>j indents current line and line below``() =
        assertText "a$bc\ndef" ">j" "    a$bc\n    def"

    [<Test>]
    let ``<j unindents current line and line below``() =
        assertText "    a$bc\n    def" "<j" "a$bc\ndef"

    [<Test;Ignore("Doesn't place caret at correct location")>]
    let ``>2j indents current line and two lines below``() =
        assertText "a$bc\ndef\nghi" ">2j" "    a$bc\n    def\n    ghi"

    [<Test>]
    let ``>gg indents to top of file``() =
        assertText "abc\ndef\ngh$i" ">gg" "    abc\n    def\n    gh$i"

    [<Test>]
    let ``V> indents line``() =
        assertText "abc\ndef\ngh$i" ">gg" "    abc\n    def\n    gh$i"

    [<Test>]
    let ``>2gg indents to line 2``() =
        assertText "abc\ndef\ngh$i" ">2gg" "abc\n    def\n    gh$i"

    [<Test>]
    let ``>2G indents to line 2``() =
        assertText "abc\ndef\ngh$i" ">2G" "abc\n    def\n    gh$i"

    [<Test>]
    let ``== autoindents line``() =
        assertText "abc\n    def\ngh$i" "==" "abc\n    def\n    g$hi"

    [<Test>]
    let ``= autoindents selection``() =
        assertText "abc\n    def\ngh$i" "V=" "abc\n    def\n    g$hi"

    [<Test>]
    let ``= autoindents multiple line selection``() =
            assertText "abc\n    de$f\n   ghi\n   jkl" "Vj=" "abc\nd$ef\nghi\n   jkl"

    [<Test>]
    let ``=gg indents to top of file``() =
        assertText "abc\n  def\n  gh$i" "=gg" "a$bc\ndef\nghi"

namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Delete tests`` =
    [<Test>]
    let ``Vjd test``() =
        let source =
            @"aaaaaa
              bb$bbbb
              cccccc
              dddddd
              eeeeee";

        let expected =
            @"aaaaaa
 $             dddddd
              eeeeee";
        assertText source "Vjd" expected

    [<Test>]
    let ``dd first line``() =
        assertText "ab$c\ndef" "dd" "d$ef"

    [<Test>]
    let ``dd last line at EOF``() =
        assertText "abc\nd$ef" "dd" "a$bc"

    [<Test>]
    let ``dd only line``() =
        assertText "a$bc" "dd" "$"

    [<Test>]
    let ``Delete char under caret``() =
        assertText "abc$def" "x" "abd$ef"

    [<Test>]
    let ``Delete char to left of caret``() =
        assertText "abc$def" "X" "ac$def"

    [<Test>]
    let ``Delete to end of line``() =
        assertText "abc$ def\nghi" "d$" "ab$\nghi"

    [<Test>]
    let ``Delete to end of document``() =
        assertText "abc\nde$f\nghi" "dG" "abc\n$"

    [<Test>]
    let ``Delete to start of document``() =
        assertText "abc\nde$f\nghi" "dgg" "g$hi"

    [<Test>]
    let ``Delete to end of line using D``() =
        assertText "abc$ def\nghi" "D" "ab$\nghi"

    [<Test>]
    let ``Deletes word``() =
        assertText "a$bc def" "dw" "d$ef"

    [<Test>]
    let ``Delete to end of word``() =
        assertText "ab$c def" "de" "a $def"

    [<Test>]
    let ``Delete to end of WORD``() =
        assertText "ab$c.def ghi" "dE" "a $ghi"

    [<Test>]
    let ``Deleting last word doesn't delete delimiter'``() =
        assertText "abc d$ef\nghi" "dw" "abc $\nghi"

    [<Test>]
    let ``Delete char to left doesn't delete past start of line``() =
        assertText "abcdef\nab$cdef" "XXX" "abcdef\nb$cdef"
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
              d$ddddd
              eeeeee";
        assertText source "Vjd" expected

    [<Test>]
    let ``Delete line and line below``() =
        let source =
            @"aaaaaa
              bb$bbbb
              cccccc
              dddddd
              eeeeee";

        let expected =
            @"aaaaaa
              d$ddddd
              eeeeee";
        assertText source "dj" expected

    [<Test>]
    let ``Delete line and next two lines``() =
        let source =
            @"aaaaaa
              bb$bbbb
              cccccc
              dddddd
              eeeeee";

        let expected =
            @"aaaaaa
              e$eeeee";
        assertText source "d2j" expected

    [<Test>]
    let ``dd first line``() =
        assertText "ab$c\n  def" "dd" "  d$ef"

    [<Test>]
    let ``dd next line is blank``() =
        assertText

            """
{
    fo$o

    bar
}
            """

            "dd"
// $ is over \n here
            """
{

$    bar
}
            """

    [<Test>]
    let ``2dd deletes 2 lines``() =
        assertText "ab$c\ndef\nghi" "2dd" "g$hi"

    [<Test>]
    let ``2ddp puts 2 lines back``() =
        assertText "abc\nde$f\nghi" "2ddp" "abc\nd$ef\nghi"

    [<Test>]
    let ``dd last line at EOF``() =
        assertText "abc\ndef\ngh$i" "dd" "abc\nd$ef"

    [<Test>]
    let ``dd only line``() =
        assertText "a$bc" "dd" "$"

    [<Test>]
    let ``Delete char under caret``() =
        assertText "abc$def" "x" "abd$ef"

    [<Test>]
    let ``Delete char at EOL``() =
        assertText "abcdef$\n" "xx" "abcd$\n"

    [<Test>]
    let ``x with multiplier stops at EOL (caret at EOL)``() =
        assertText "abcdef$\n" "4x" "abcde$\n"

    [<Test>]
    let ``x with multiplier stops at EOL``() =
        assertText "ab$cdef\n" "100x" "a$\n"

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
    let ``Delete to end of line from start keeps caret on current line``() =
        assertText "abc\nd$ef\nghi" "D" "abc\n\n$ghi"

    [<Test>]
    let ``Deletes word``() =
        assertText "a$bc     def" "dw" "d$ef"

    [<Test>]
    let ``Delete to end of word``() =
        assertText "ab$c def" "de" "a $def"

    [<Test>]
    let ``Delete to end of word does not include dot``() =
        assertText "open Mon$o.Addins" "de" "open Mo.$Addins"

    [<Test>]
    let ``Delete to end of word includes dot``() =
        assertText "open Mono$.Addins" "de" "open MonA$ddins"

    [<Test>]
    let ``Delete word does not include dot``() =
        assertText "open Mo$no.Addins" "dw" "open M.$Addins"

    [<Test>]
    let ``Delete to end of WORD``() =
        assertText "ab$c.def ghi" "dE" "a $ghi"

    [<Test>]
    let ``Deleting last word doesn't delete delimiter'``() =
        assertText "abc d$ef  \nghi" "dw" "abc $\nghi"

    [<Test>]
    let ``Deleting last word touching EOL doesn't delete delimiter'``() =
        assertText "abc d$ef\nghi" "dw" "abc $\nghi"

    [<Test>]
    let ``Delete char to left doesn't delete past start of line``() =
        assertText "abcdef\nab$cdef" "XXX" "abcdef\nb$cdef"

    [<Test>]
    let ``dw last word at EOF``() =
        assertText "a$bc" "dw" "$"

    [<Test>]
    let ``dw to brace #167``() =
        assertText "abc\n $  {" "dw" "abc\n{$"

    [<Test>]
    let ``d]) deletes to next unmatched )``() =
        assertText "if (a$ == (b)c)" "d])" "if ($"
    [<Test>]
    let ``dib deletes nested brackets``() =
        assertText "(fo$o (bar (foo) )  )" "dib" "()$"

    [<Test>]
    let ``dib deletes nested brackets backwards``() =
        assertText "(foo (bar (foo) ) $ )" "dib" "()$"
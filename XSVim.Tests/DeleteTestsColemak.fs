namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Delete tests Colemak`` =
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
        assertColemakText source "Vns" expected

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
        assertColemakText source "sn" expected

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
        assertColemakText source "s2n" expected

    [<Test>]
    let ``dd first line``() =
        assertColemakText "ab$c\n  def" "ss" "  d$ef"

    [<Test>]
    let ``dd next line is blank``() =
        assertColemakText

            """
{
    fo$o

    bar
}
            """

            "ss"
// $ is over \n here
            """
{

$    bar
}
            """

    [<Test>]
    let ``2dd deletes 2 lines``() =
        assertColemakText "ab$c\ndef\nghi" "2ss" "g$hi"

    [<Test>]
    let ``2ddp puts 2 lines back``() =
        assertColemakText "abc\nde$f\nghi" "2ss;" "abc\nd$ef\nghi"

    [<Test>]
    let ``dd last line at EOF``() =
        assertColemakText "abc\ndef\ngh$i" "ss" "abc\nd$ef"

    [<Test>]
    let ``dd only line``() =
        assertColemakText "a$bc" "ss" "$"

    [<Test>]
    let ``Delete char under caret``() =
        assertColemakText "abc$def" "x" "abd$ef"

    [<Test>]
    let ``Delete char at EOL``() =
        assertColemakText "abcdef$\n" "xx" "abcd$\n"

    [<Test>]
    let ``x with multiplier stops at EOL (caret at EOL)``() =
        assertColemakText "abcdef$\n" "4x" "abcde$\n"

    [<Test>]
    let ``x with multiplier stops at EOL``() =
        assertColemakText "ab$cdef\n" "100x" "a$\n"

    [<Test>]
    let ``Delete char to left of caret``() =
        assertColemakText "abc$def" "X" "ac$def"

    [<Test>]
    let ``Delete to end of line``() =
        assertColemakText "abc$ def\nghi" "s$" "ab$\nghi"

    [<Test>]
    let ``Delete to end of document``() =
        assertColemakText "abc\nde$f\nghi" "sD" "abc\n$"

    [<Test>]
    let ``Delete to start of document``() =
        assertColemakText "abc\nde$f\nghi" "sdd" "g$hi"

    [<Test>]
    let ``Delete to end of line using D``() =
        assertColemakText "abc$ def\nghi" "S" "ab$\nghi"

    [<Test>]
    let ``Delete to end of line from start keeps caret on current line``() =
        assertColemakText "abc\nd$ef\nghi" "S" "abc\n\n$ghi"

    [<Test>]
    let ``Deletes word``() =
        assertColemakText "a$bc     def" "sw" "d$ef"

    [<Test>]
    let ``Delete to end of word``() =
        assertColemakText "ab$c def" "sf" "a $def"

    [<Test>]
    let ``Delete to end of word does not include dot``() =
        assertColemakText "open Mon$o.Addins" "sf" "open Mo.$Addins"

    [<Test>]
    let ``Delete to end of word includes dot``() =
        assertColemakText "open Mono$.Addins" "sf" "open MonA$ddins"

    [<Test>]
    let ``Delete word does not include dot``() =
        assertColemakText "open Mo$no.Addins" "sw" "open M.$Addins"

    [<Test>]
    let ``Delete to end of WORD``() =
        assertColemakText "ab$c.def ghi" "sF" "a $ghi"

    [<Test>]
    let ``Deleting last word doesn't delete delimiter'``() =
        assertColemakText "abc d$ef  \nghi" "sw" "abc $\nghi"

    [<Test>]
    let ``Deleting last word touching EOL doesn't delete delimiter'``() =
        assertColemakText "abc d$ef\nghi" "sw" "abc $\nghi"

    [<Test>]
    let ``Delete char to left doesn't delete past start of line``() =
        assertColemakText "abcdef\nab$cdef" "XXX" "abcdef\nb$cdef"

    [<Test>]
    let ``dw last word at EOF``() =
        assertColemakText "a$bc" "sw" "$"

    [<Test>]
    let ``dw to brace #167``() =
        assertColemakText "abc\n $  {" "sw" "abc\n{$"

    [<Test>]
    let ``d]) deletes to next unmatched )``() =
        assertColemakText "if (a$ == (b)c)" "s])" "if ($"
    [<Test>]
    let ``dib deletes nested brackets``() =
        assertColemakText "(fo$o (bar (foo) )  )" "sub" "()$"

    [<Test>]
    let ``dib deletes nested brackets backwards``() =
        assertColemakText "(foo (bar (foo) ) $ )" "sub" "()$"

    [<Test>]
    let ``dib outside brackets does nothing``() =
        assertColemakText "don't crash$ me" "sub" "don't crash$ me"
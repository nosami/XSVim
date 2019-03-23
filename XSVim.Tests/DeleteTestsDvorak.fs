namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Delete tests Dvorak`` =
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
        assertDvorakText source "Khe" expected

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
        assertDvorakText source "eh" expected

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
        assertDvorakText source "e2h" expected

    [<Test>]
    let ``dd first line``() =
        assertDvorakText "ab$c\n  def" "ee" "  d$ef"

    [<Test>]
    let ``dd next line is blank``() =
        assertDvorakText

            """
{
    fo$o

    bar
}
            """

            "ee"
// $ is over \n here
            """
{

$    bar
}
            """

    [<Test>]
    let ``2dd deletes 2 lines``() =
        assertDvorakText "ab$c\ndef\nghi" "2ee" "g$hi"

    [<Test>]
    let ``2ddp puts 2 lines back``() =
        assertDvorakText "abc\nde$f\nghi" "2eel" "abc\nd$ef\nghi"

    [<Test>]
    let ``dd last line at EOF``() =
        assertDvorakText "abc\ndef\ngh$i" "ee" "abc\nd$ef"

    [<Test>]
    let ``dd only line``() =
        assertDvorakText "a$bc" "ee" "$"

    [<Test>]
    let ``Delete char under caret``() =
        assertDvorakText "abc$def" "q" "abd$ef"

    [<Test>]
    let ``Delete char at EOL``() =
        assertDvorakText "abcdef$\n" "qq" "abcd$\n"

    [<Test>]
    let ``x with multiplier stops at EOL (caret at EOL)``() =
        assertDvorakText "abcdef$\n" "4q" "abcde$\n"

    [<Test>]
    let ``x with multiplier stops at EOL``() =
        assertDvorakText "ab$cdef\n" "100q" "a$\n"

    [<Test>]
    let ``Delete char to left of caret``() =
        assertDvorakText "abc$def" "Q" "ac$def"

    [<Test>]
    let ``Delete to end of line``() =
        assertDvorakText "abc$ def\nghi" "e$" "ab$\nghi"

    [<Test>]
    let ``Delete to end of document``() =
        assertDvorakText "abc\nde$f\nghi" "eI" "abc\n$"

    [<Test>]
    let ``Delete to start of document``() =
        assertDvorakText "abc\nde$f\nghi" "eii" "g$hi"

    [<Test>]
    let ``Delete to end of line using D``() =
        assertDvorakText "abc$ def\nghi" "E" "ab$\nghi"

    [<Test>]
    let ``Delete to end of line from start keeps caret on current line``() =
        assertDvorakText "abc\nd$ef\nghi" "E" "abc\n\n$ghi"

    [<Test>]
    let ``Deletes word``() =
        assertDvorakText "a$bc     def" "e," "d$ef"

    [<Test>]
    let ``Delete to end of word``() =
        assertDvorakText "ab$c def" "e." "a $def"

    [<Test>]
    let ``Delete to end of word does not include dot``() =
        assertDvorakText "open Mon$o.Addins" "e." "open Mo.$Addins"

    [<Test>]
    let ``Delete to end of word includes dot``() =
        assertDvorakText "open Mono$.Addins" "e." "open MonA$ddins"

    [<Test>]
    let ``Delete word does not include dot``() =
        assertDvorakText "open Mo$no.Addins" "e," "open M.$Addins"

    [<Test>]
    let ``Delete to end of WORD``() =
        assertDvorakText "ab$c.def ghi" "e>" "a $ghi"

    [<Test>]
    let ``Deleting last word doesn't delete delimiter'``() =
        assertDvorakText "abc d$ef  \nghi" "e," "abc $\nghi"

    [<Test>]
    let ``Deleting last word touching EOL doesn't delete delimiter'``() =
        assertDvorakText "abc d$ef\nghi" "e," "abc $\nghi"

    [<Test>]
    let ``Delete char to left doesn't delete past start of line``() =
        assertDvorakText "abcdef\nab$cdef" "QQQ" "abcdef\nb$cdef"

    [<Test>]
    let ``dw last word at EOF``() =
        assertDvorakText "a$bc" "e," "$"

    [<Test>]
    let ``dw to brace #167``() =
        assertDvorakText "abc\n $  {" "e," "abc\n{$"

    [<Test>]
    let ``d]) deletes to next unmatched )``() =
        assertDvorakText "if (a$ == (b)c)" "e=)" "if ($"

    [<Test>]
    let ``dib deletes nested brackets``() =
        assertDvorakText "(fo$o (bar (foo) )  )" "ecx" "()$"

    [<Test>]
    let ``dib deletes nested brackets backwards``() =
        assertDvorakText "(foo (bar (foo) ) $ )" "ecx" "()$"

    [<Test>]
    let ``dib outside brackets does nothing``() =
        assertDvorakText "don't crash$ me" "ecx" "don't crash$ me"
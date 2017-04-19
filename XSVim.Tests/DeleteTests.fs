namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Delete tests`` =
    [<Test>]
    let ``dd test``() =
        //TODO - caret finishes in wrong position
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
        //Assert.False true
        assertText source "Vjd" expected

    [<Test>]
    let ``Delete char under caret``() =
        assertText "abc$def" "x" "abd$ef"

    [<Test>]
    let ``Delete char to left of caret``() =
        assertText "abc$def" "X" "ac$def"

    [<Test>]
    let ``Delete to end of line``() =
        assertText "abc$ def\nghi" "d$" "ab\n$ghi"

    [<Test>]
    let ``Delete to end of line using D``() =
        assertText "abc$ def\nghi" "D" "ab\n$ghi"

    [<Test>]
    let ``Delete char to left doesn't delete past start of line``() =
        assertText "abcdef\nab$cdef" "XXX" "abcdef\nb$cdef"
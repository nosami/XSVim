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
        test source "Vjd" expected

    [<Test>]
    let ``Delete char under caret``() =
        test "abc$def" "x" "abd$ef"

    [<Test>]
    let ``Delete char to left of caret``() =
        test "abc$def" "X" "ac$def"
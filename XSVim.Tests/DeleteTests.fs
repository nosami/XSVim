namespace XSVim.Tests

open XSVim
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
              dddddd
              eeeeee";
        //Assert.False true
        test source "Vjd" expected ""

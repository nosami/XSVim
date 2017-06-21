namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Ex mode tests`` =
    [<Test>]
    let ``/ searches for word``() =
        assertText "ab$c abc" "/abc<ret>" "abc a$bc"

    [<Test>]
    let ``n searches for next word``() =
        assertText "ab$c abc abc" "/abc<ret>n" "abc abc a$bc"
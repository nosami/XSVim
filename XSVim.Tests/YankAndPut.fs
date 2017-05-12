namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Yank and put tests`` =
    [<Test>]
    let ``Yanking puts cursor at original position before selection was made``() =
        assertText "a$bc" "vly" "a$bc"

    [<Test>]
    let ``Should put line at last line``() =
        assertText "  abc$\ndef" "yyjp" "  abc\ndef\n  a$bc"

    [<Test>]
    let ``Should put ab after``() =
        assertText "a$bc" "vldp" "cab$"

    [<Test>]
    let ``Should put abc over selection in visual mode``() =
        assertText "a$bc" "vlyvllp" "ab$"

    [<Test>]
    let ``P acts like p in visual mode``() =
        assertText "a$bc" "vlyvllP" "ab$"

namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Marker tests`` =
    [<Test>]
    let ``ma adds marker a``() =
        assertText "a$bc" "mall`a" "a$bc"

    [<Test>]
    let ``'a moves to start of line of marker a``() =
        assertText "123 a$bc" "mall'a" "1$23 abc"

    [<Test>]
    let ``'. jumps to last edit line``() =
        assertText "  123 a$bc" "i<esc>'." "  1$23 abc"

    [<Test>]
    let ``'' jumps to last jump location line``() =
        assertText "ab$c\ndef" "G''" "a$bc\ndef"

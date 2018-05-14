namespace XSVim.Tests
open NUnit.Framework
open XSVim

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

    [<Test>]
    let ``Deletes from 4 up to marker a``() =
        assertText "1234XXXXXXXa$5678" "maF4d`a" "123a$5678"

    [<Test>]
    let ``Selects from 4 up to marker a``() =
        let _ = test "1234XXXXXXXa$5678" "maF4v`ay" 
        Vim.registers.[EmptyRegister].content
        |> should equal "4XXXXXXXa"

    [<Test>]
    let ``Deletes linewise between marker a and marker b``() =
        assertText
            """
.........
aaaaa$aaaa
.........
.........
bbbbbbbbb
.........
            """

              "ma/b<ret>mb:'a,'bd<ret>"

            """
.........
.$........
            """
namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Marker tests Colemak`` =
    [<Test>]
    let ``ma adds marker a``() =
        assertColemakText "a$bc" "maii`a" "a$bc"

    [<Test>]
    let ``'a moves to start of line of marker a``() =
        assertColemakText "123 a$bc" "maii'a" "1$23 abc"

    [<Test>]
    let ``'. jumps to last edit line``() =
        assertColemakText "  123 a$bc" "u<esc>'." "  1$23 abc"

    [<Test>]
    let ``'' jumps to last jump location line``() =
        assertColemakText "ab$c\ndef" "D''" "a$bc\ndef"

    [<Test>]
    let ``Deletes from 4 up to marker a``() =
        assertColemakText "1234XXXXXXXa$5678" "maT4s`a" "123a$5678"

    [<Test>]
    let ``Selects from 4 up to marker a``() =
        let _ = testColemak "1234XXXXXXXa$5678" "maT4v`aj" 
        Vim.registers.[EmptyRegister].content
        |> should equal "4XXXXXXXa"

    [<Test>]
    let ``Deletes linewise between marker a and marker b``() =
        assertColemakText
            """
.........
aaaaa$aaaa
.........
.........
bbbbbbbbb
.........
            """

              "ma/b<ret>mbO'a,'bd<ret>"

            """
.........
.$........
            """
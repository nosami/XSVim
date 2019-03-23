namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Marker tests Dvorak`` =
    [<Test>]
    let ``ma adds marker a``() =
        assertDvorakText "a$bc" "mann`a" "a$bc"

    [<Test>]
    let ``'a moves to start of line of marker a``() =
        assertDvorakText "123 a$bc" "mann-a" "1$23 abc"

    [<Test>]
    let ``'. jumps to last edit line``() =
        assertDvorakText "  123 a$bc" "c<esc>-v" "  1$23 abc"

    [<Test>]
    let ``'' jumps to last jump location line``() =
        assertDvorakText "ab$c\ndef" "I--" "a$bc\ndef"

    [<Test>]
    let ``Deletes from 4 up to marker a``() =
        assertDvorakText "1234XXXXXXXa$5678" "maU4e`a" "123a$5678"

    [<Test>]
    let ``Selects from 4 up to marker a``() =
        let _ = testDvorak "1234XXXXXXXa$5678" "maU4k`af" 
        Vim.registers.[EmptyRegister].content
        |> should equal "4XXXXXXXa"

    [<Test>]
    let ``Deletes linewise between marker a and marker b``() =
        assertDvorakText
            """
.........
aaaaa$aaaa
.........
.........
bbbbbbbbb
.........
            """

              "mazb<ret>mbS'a,'bd<ret>"

            """
.........
.$........
            """
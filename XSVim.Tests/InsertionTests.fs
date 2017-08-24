namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Insertion tests`` =
    [<Test>]
    let ``'O' should insert line above``() =
        //TODO: 'O' is broken on the top line
        assertText " \n a$bcdef" "O" " \n |\n abcdef"

    [<Test>]
    let ``'O' inserts line above``() =
        assertText "abc$def\n" "O" "|\nabcdef\n"
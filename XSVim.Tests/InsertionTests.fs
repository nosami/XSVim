namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Insertion tests`` =
    [<Test>]
    let ``'O' should insert line above``() =
        //TODO: 'O' is broken on the top line
        assertText " \n a$bcdef" "O" "\n|\n abcdef"

    [<Test>]
    let ``'O' should work on the first line``()=
        assertText " a$bcdef" "O" "|\n abcdef"
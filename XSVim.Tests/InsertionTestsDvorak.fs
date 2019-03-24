namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Insertion tests Dvorak`` =
    [<Test>]
    let ``'O' should insert line above``() =
        //TODO: 'O' is broken on the top line
        assertDvorakText " \n a$bcdef" "R" " \n |\n abcdef"

    [<Test>]
    let ``'O' inserts line above``() =
        assertDvorakText "abc$def\n" "R" "|\nabcdef\n"

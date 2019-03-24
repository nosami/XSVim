namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Insertion tests Colemak`` =
    [<Test>]
    let ``'O' should insert line above``() =
        //TODO: 'O' is broken on the top line
        assertColemakText " \n a$bcdef" "Y" " \n |\n abcdef"

    [<Test>]
    let ``'O' inserts line above``() =
        assertColemakText "abc$def\n" "Y" "|\nabcdef\n"

namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Insertion tests`` =
    [<Test>]
    let ``'O' should insert line above``() =
        assertText " a$bcdef" "O" "|\n abcdef"

namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Indentation tests`` =
    [<Test>]
    let ``>> indents right in normal mode``() =
        assertText "a$bc" ">>" "    a$bc"

    [<Test>]
    let ``> indents right in visual mode``() =
        assertText "a$bc" "V>" "    a$bc"

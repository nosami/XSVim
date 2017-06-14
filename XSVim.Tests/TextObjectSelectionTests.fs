namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Text object selection tests`` =

    // Tests for aw. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#aw

    [<Test>]
    let ``daw on a word``() =
        assertText "word1   word2$   word3" "daw" "word1   w$ord3" // three spaces before word2 preserved, two after removed

    [<Test>]
    let ``daw from whitespace before a word``() =
        assertText "word1  $  word2   word3" "daw" "word1 $  word3" // four spaces before word2 removed, three after preserved

    [<Test>]
    let ``aw stops searching at EOL``() =
        assertText "word1$    \n word2" "daw" "\n$ word2"

    [<Test>]
    let ``caw on a word``() =
        assertText "word1   word2$  word3" "caw" "word1   |word3"

    
    // Tests for quoted strings. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#a`

    [<TestCase("\"")>]
    [<TestCase("'")>]
    [<TestCase("`")>]
    let ``ci<q> before quoted string`` q =
        assertText ("var$ a = " + q + "value" + q) ("ci" + q) ("var a = " + q + "|" + q)

    [<TestCase("\"")>]
    [<TestCase("'")>]
    [<TestCase("`")>]
    let ``ci<q> inside quoted string`` q =
        assertText ("var a = " + q + "value$" + q) ("ci" + q) ("var a = " + q + "|" + q)

    // TODO: di" tests cheat. VIM puts the cursor at the closing quote. The resulting string should be:
    // var a = ""$"
    [<TestCase("\"")>]
    [<TestCase("'")>]
    [<TestCase("`")>]
    let ``di<q> before quoted string`` q =
        assertText ("var$ a = " + q + "value" + q) ("di" + q) ("var a = " + q + "$" + q) 

    [<TestCase("\"")>]
    [<TestCase("'")>]
    [<TestCase("`")>]
    let ``di<q> inside quoted string`` q =
        assertText ("var a = " + q + "value$" + q) ("di" + q) ("var a = " + q + "$" + q)

    // TODO: ca" and da" tests cheat. The commands should delete the white space after the closing quote
    [<TestCase("\"")>]
    [<TestCase("'")>]
    [<TestCase("`")>]
    let ``ca<q> before quoted string`` q =
        assertText ("var$ a = " + q + "value" + q) ("ca" + q) ("var a = |") 

    [<TestCase("\"")>]
    [<TestCase("'")>]
    [<TestCase("`")>]
    let ``ca<q> inside quoted string`` q =
        assertText ("var a = " + q + "value$" + q) ("ca" + q) ("var a = |") 

    [<TestCase("\"")>]
    [<TestCase("'")>]
    [<TestCase("`")>]
    let ``da<q> before quoted string`` q =
        assertText ("var$ a = " + q + "value" + q) ("da" + q) ("var a = $") 

    [<TestCase("\"")>]
    [<TestCase("'")>]
    [<TestCase("`")>]
    let ``da<q> inside quoted string`` q =
        assertText ("var a = " + q + "value$" + q) ("da" + q) ("var a = $") 

    [<Test>]
    [<Ignore "Didn't find out how to signal NoOp from getRange">]
    let ``ci" does nothing when no quoted string on line``() =
        assertText "var$ a = b\n" "ci\"" "var$ a = b\n"
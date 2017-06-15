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
    // Handing of different quotes is identical. The tests alternate between ', " and `
    [<Test>]
    let ``ci' before quoted string``()  =
        assertText "var$ a = 'value'" "ci'" "var a = '|'"

    [<Test>]
    let ``ci" inside quoted string``() =
        assertText "var a = \"value$\"" "ci\"" "var a = \"|\""

    [<Test>]
    [<Ignore "This is the correct behaviour. VIM puts the cursor on the closing quote.">]
    let ``PROPER: di` before quoted string``() =
        assertText "var$ a = `value`" "di`" "var a = ``$"

    [<Test>]
    let ``di` before quoted string``() =
        assertText "var$ a = `value`" "di`" "var a = `$`"

    [<Test>]
    let ``di' inside quoted string``() =
        assertText "var a = 'value$'" "di'" "var a = '$'"

    // TODO: ca" and da" tests cheat. The commands should delete the white space after the closing quote
    [<Test>]
    let ``ca" before quoted string``()  =
        assertText "var$ a = \"value\"" "ca\"" "var a = |" 

    [<Test>]
    let ``ca` inside quoted string``() =
        assertText "var a = `value$`" "ca`" "var a = |" 

    [<Test>]
    let ``da' before quoted string``() =
        assertText "var$ a = 'value'" "da'" "var a = $" 

    [<Test>]
    let ``da` inside quoted string``() =
        assertText "var a = `value$`" "da`" "var a = $" 

    [<Test>]
    [<Ignore "Didn't find out how to signal NoOp from getRange">]
    let ``ci" does nothing when no quoted text on line``() =
        assertText "var$ a = b\n" "ci\"" "var$ a = b\n"

    [<Test>]
    [<Ignore "Handling of escaped quotes is not implemented">]
    let ``ci" handles escaped quote``() =
        assertText """ var$ a = "\"" """ "ci\"" " var a = \"$\" "

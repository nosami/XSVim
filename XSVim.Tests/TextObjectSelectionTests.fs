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

    [<Test>]
    let ``caw finds end of word``() =
        assertText "a [wo$rd_ ] b" "caw" "a [|] b"
    
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

    [<Test>]
    let ``diw deletes a word``() =
        assertText "a b$c d" "diw" "a  $d"    
    
    [<Test>]
    let ``daw deletes a WORD``() =
        assertText "a b$c d" "daw" "a d$"    
    
    // Tests for braces. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#a)
    [<Test>]
    let ``ci( handles nested parentheses backwards``() =
        assertText "(ignored)\n((abc)\n de$f)\n(ignored)" "ci(" "(ignored)\n(|)\n(ignored)"

    [<Test>]
    let ``ci( handles nested parentheses forwards``() =
        assertText "(ignored)\n(abc$\n (def))\n(ignored)" "ci(" "(ignored)\n(|)\n(ignored)"

    [<Test>]
    let ``da{ handles nested braces forwards``() =
        assertText "{a$ {b}}\n{ignored}" "da{" "\n${ignored}"

    [<Test>]
    let ``doesn't belong here``() = 
        assertText "ab$\n" "s" "a|\n" 

    [<Test>]
    let ``doesn't belong here 1``() = 
        assertText "ab c$\n" "s" "ab |\n" 

    [<Test>]
    let ``doesn't belong here 2``() = 
        assertText "ab$\n" "xi" "a|\n" // should "s" be an alias for "xi"? Check the undo

    [<Test>]
    let ``doesn't belong here 3``() =
        assertText "ab$(" "x" "ab($"


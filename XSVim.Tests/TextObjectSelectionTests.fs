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

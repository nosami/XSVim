namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Text object selection tests Dvorak`` =
    module aw =
        // Tests for aw. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#aw
        [<Test>]
        let ``daw deletes around word``() =
            assertDvorakText "a b$c d" "ea," "a d$"

        [<Test>]
        let ``daw on a word``() =
            assertDvorakText "word1   word2$   word3" "ea," "word1   w$ord3" // three spaces before word2 preserved, two after removed

        [<Test>]
        let ``daw from whitespace before a word``() =
            assertDvorakText "word1  $  word2   word3" "ea," "word1 $  word3" // four spaces before word2 removed, three after preserved

        [<Test>]
        let ``daw puts caret at next character``() =
            assertDvorakText "a b$ ." "ea," "a .$"

        [<Test>]
        let ``daw deletes whitespace only``() =
            assertDvorakText "a. b$;" "ea," "a.;$"

        [<Test>]
        let ``daw deletes leading whitespace if there's no trailing``() =
            assertDvorakText "a b$" "ea," "a$" //

        [<Test>]
        let ``daw stops searching at EOL``() =
            assertDvorakText "word1$    \n word2" "ea," "\n$ word2"

        [<Test>]
        let ``caw on a word``() =
            assertDvorakText "word1   word2$  word3" "ja," "word1   |word3"

        [<Test>]
        let ``caw finds end of word``() =
            assertDvorakText "a [wo$rd_ ] b" "ja," "a [|] b"

        [<Test>]
        let ``caw on a word with digits and underscores``() =
            assertDvorakText ".a_8$ b" "ja," ".|b"

        [<Test>]
        let ``daw on a word delimited by punctuation``() =
            assertDvorakText "w1.w2$.w3" "ea," "w1..$w3"

        [<Test>]
        let ``caw on a word delimited by punctuation``() =
            assertDvorakText "w1(w2$)w3" "ja," "w1(|)w3"

        [<Test>]
        let ``daw on sequence of other characters``() =
            assertDvorakText "a ~!@#%^$&*=+:;?/<>(){}b " "ea," "ab$ "

        [<Test>]
        let ``daw from white space at EOL extends to next line``() =
            assertDvorakText "a $ \n b" "ea," "a$"

    module aW =
        // Tests for aW. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#aW
        [<Test>]
        let ``daW deletes a WORD``() =
            assertDvorakText "a W.W$ b" "ea<" "a b$"

        [<Test>]
        let ``daW removes trailing space``() =
            assertDvorakText "a W.W$ " "ea<" "a $"

        [<Test>]
        let ``daW removes leading space if no trailing``() =
            assertDvorakText "a W.W$" "ea<" "a$"

        [<Test>]
        let ``caW changes a WORD``() =
            assertDvorakText "a W.W$ b" "ja<" "a |b"

    module iw =
        // Tests for iw. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#iw
        [<Test>]
        let ``diw deletes inside word``() =
            assertDvorakText "a b$c d" "ec," "a  $d"

        [<Test>]
        let ``diw on sequence of other characters``() =
            assertDvorakText "a ~!@#%^&*=+:;?/<>(){}$b " "ec," "a b$ "

        [<Test>]
        let ``ciw changes a word``() =
            assertDvorakText "a b$ c" "jc," "a | c"

        [<Test>]
        let ``diw stops at non-word character``() =
            assertDvorakText "%b$_123." "ec," "%.$"

        [<Test>]
        let ``diw deletes whitespace``() =
            assertDvorakText "a   $   b" "ec," "ab$"

        [<Test>]
        let ``diw does not extend to next line``() =
            assertDvorakText "a $ \n b" "ec," "a$\n b"

        [<Test>]
        let ``ciw changes inside word``() =
            assertDvorakText "a b$c d" "jc," "a | d"

        // Tests for iW.
        [<Test>]
        let ``diW deletes a WORD``() =
            assertDvorakText "a W.W$ b" "ec<" "a  $b"

        [<Test>]
        let ``ciW changes a WORD``() =
            assertDvorakText "a W.W2$ b" "jc<" "a | b"

    // Tests for quoted strings. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#a`
    // Handling of different quotes is identical. The tests alternate between ', " and `
    [<Test>]
    let ``ci' before quoted string``()  =
        assertDvorakText "var$ a = 'value'" "jc-" "var a = '|'"

    [<Test>]
    let ``ci" inside quoted string``() =
        assertDvorakText "var a = \"value$\"" "jc_" "var a = \"|\""

    [<Test>]
    let ``di` before quoted string``() =
        assertDvorakText "var$ a = `value`" "ec`" "var a = ``$"

    [<Test>]
    let ``di' inside quoted string``() =
        assertDvorakText "var a = 'value$'" "ec-" "var a = ''$"

    // TODO: ca" and da" tests cheat. The commands should delete the white space after the closing quote
    [<Test>]
    let ``ca" before quoted string``()  =
        assertDvorakText "var$ a = \"value\"" "ja_" "var a = |"

    [<Test>]
    let ``ca` inside quoted string``() =
        assertDvorakText "var a = `value$`" "ja`" "var a = |"

    [<Test>]
    let ``da' before quoted string``() =
        assertDvorakText "var$ a = 'value'" "ea-" "var a = $"

    [<Test>]
    let ``da` inside quoted string``() =
        assertDvorakText "var a = `value$`" "ea`" "var a = $"

    [<Test>]
    [<Ignore "Didn't find out how to signal NoOp from getRange">]
    let ``ci" does nothing when no quoted text on line``() =
        assertDvorakText "var$ a = b\n" "jc_" "var$ a = b\n"

    [<Test>]
    [<Ignore "Handling of escaped quotes is not implemented">]
    let ``ci" handles escaped quote``() =
        assertDvorakText """ var$ a = "\"" """ "jc_" " var a = \"$\" "

    // Tests for braces. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#a)
    [<Test>]
    let ``ci( handles nested parentheses backwards``() =
        assertDvorakText "(ignored)\n((abc)\n de$f)\n(ignored)" "jc(" "(ignored)\n(|)\n(ignored)"

    [<Test>]
    let ``ci( handles nested parentheses forwards``() =
        assertDvorakText "(ignored)\n(abc$\n (def))\n(ignored)" "jc(" "(ignored)\n(|)\n(ignored)"

    [<Test>]
    let ``da{ handles nested braces forwards``() =
        assertDvorakText "{a$ {b}}\n{ignored}" "ea?" "\n${ignored}"

    // Tags. Reeference: http://vimdoc.sourceforge.net/htmldoc/motion.html#tag-blocks
    module at =
        [<Test>]
        let ``dat from tag content``() =
            assertDvorakText "<a><b>val$</b></a>" "eay" "<a><$/a>"

        [<Test>]
        let ``dat from tag declaration``() =
            assertDvorakText "<a><b$>val</b></a>" "eay" "<a><$/a>"

        [<Test>]
        let ``dat ignores unclosed tag``() =
            assertDvorakText "<a$>val" "eay" "<a$>val"

        [<Test>]
        let ``dat ignores self-closing tag``() =
            assertDvorakText "<a><b$/></a>" "eay" "$"

        [<Test>]
        let ``dat handles tags with attributes``() =
            assertDvorakText "<a><b atr='val'>val$</b></a>" "eay" "<a><$/a>"

        [<Test>]
        let ``dat handles tags with namespaces``() =
            assertDvorakText "<a><ns:b>val$</ns:b></a>" "eay" "<a><$/a>"

        [<Test>]
        let ``dat finds unmatched closing tag``() =
            assertDvorakText "<a><b$><b></b></b></a>" "eay" "<a><$/a>"

        [<Test>]
        let ``dat when closing > is on separate line``() =
            assertDvorakText "<a\n>val$</a>" "eay" "$"

        [<Test>]
        let ``cat from tag content``() =
            assertDvorakText "<a><b>val$</b></a>" "jay" "<a>|</a>"

        [<Test>]
        let ``cat from tag declaration``() =
            assertDvorakText "<a><b$>val</b></a>" "jay" "<a>|</a>"

        [<Test>]
        [<Ignore("Need to find out how to signal NoOp from getRange")>]
        let ``cat ignores unclosed tag``() =
            assertDvorakText "<a$>val" "jay" "<a$>val"

        [<Test>]
        let ``cat handles tags with attributes``() =
            assertDvorakText "<a><b atr='val'>val$</b></a>" "jay" "<a>|</a>"

        [<Test>]
        let ``cat ignores self-closing tag``() =
            assertDvorakText "<a><b$/></a>" "jay" "|"

    module it =
        [<Test>]
        let ``dit from tag content``() =
            assertDvorakText "<a><b>val$</b></a>" "ecy" "<a><b><$/b></a>"

        [<Test>]
        let ``dit from tag declaration``() =
            assertDvorakText "<a><b$>val</b></a>" "ecy" "<a><b><$/b></a>"

        [<Test>]
        let ``dit finds unmatched tag``() =
            assertDvorakText "<a><b$><b></b></b></a>" "ecy" "<a><b><$/b></a>"

        [<Test>]
        let ``dit outside a tag does nothing``() =
            assertDvorakText " $ <a>val</a>" "ecy" " $ <a>val</a>"

        [<Test>]
        let ``cit from tag content``() =
            assertDvorakText "<a><b>val$</b></a>" "jcy" "<a><b>|</b></a>"

        [<Test>]
        let ``cit from tag declaration``() =
            assertDvorakText "<a><b$>val</b></a>" "jcy" "<a><b>|</b></a>"

        [<Test>]
        let ``cit finds unmatched tag``() =
            assertDvorakText "<a><b$><b></b></b></a>" "jcy" "<a><b>|</b></a>"

        [<Test>]
        let ``cit when closing > is on separate line``() =
            assertDvorakText "<a\n>val$</a>" "jcy" "<a\n>|</a>"

        [<Test>]
        [<Ignore("Need to find out how to signal NoOp from getRange")>]
        let ``cit outside a tag does nothing``() =
            assertDvorakText " $ <a>val</a>" "jcy" " $ <a>val</a>"


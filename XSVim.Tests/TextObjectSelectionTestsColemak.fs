namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Text object selection tests Colemak`` =
    module aw =
        // Tests for aw. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#aw
        [<Test>]
        let ``daw deletes around word``() =
            assertColemakText "a b$c d" "saw" "a d$"

        [<Test>]
        let ``daw on a word``() =
            assertColemakText "word1   word2$   word3" "saw" "word1   w$ord3" // three spaces before word2 preserved, two after removed

        [<Test>]
        let ``daw from whitespace before a word``() =
            assertColemakText "word1  $  word2   word3" "saw" "word1 $  word3" // four spaces before word2 removed, three after preserved

        [<Test>]
        let ``daw puts caret at next character``() =
            assertColemakText "a b$ ." "saw" "a .$"

        [<Test>]
        let ``daw deletes whitespace only``() =
            assertColemakText "a. b$;" "saw" "a.;$"

        [<Test>]
        let ``daw deletes leading whitespace if there's no trailing``() =
            assertColemakText "a b$" "saw" "a$" //

        [<Test>]
        let ``daw stops searching at EOL``() =
            assertColemakText "word1$    \n word2" "saw" "\n$ word2"

        [<Test>]
        let ``caw on a word``() =
            assertColemakText "word1   word2$  word3" "caw" "word1   |word3"

        [<Test>]
        let ``caw finds end of word``() =
            assertColemakText "a [wo$rd_ ] b" "caw" "a [|] b"

        [<Test>]
        let ``caw on a word with digits and underscores``() =
            assertColemakText ".a_8$ b" "caw" ".|b"

        [<Test>]
        let ``daw on a word delimited by punctuation``() =
            assertColemakText "w1.w2$.w3" "saw" "w1..$w3"

        [<Test>]
        let ``caw on a word delimited by punctuation``() =
            assertColemakText "w1(w2$)w3" "caw" "w1(|)w3"

        [<Test>]
        let ``daw on sequence of other characters``() =
            assertColemakText "a ~!@#%^$&*=+:;?/<>(){}b " "saw" "ab$ "

        [<Test>]
        let ``daw from white space at EOL extends to next line``() =
            assertColemakText "a $ \n b" "saw" "a$"

    module aW =
        // Tests for aW. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#aW
        [<Test>]
        let ``daW deletes a WORD``() =
            assertColemakText "a W.W$ b" "saW" "a b$"

        [<Test>]
        let ``daW removes trailing space``() =
            assertColemakText "a W.W$ " "saW" "a $"

        [<Test>]
        let ``daW removes leading space if no trailing``() =
            assertColemakText "a W.W$" "saW" "a$"

        [<Test>]
        let ``caW changes a WORD``() =
            assertColemakText "a W.W$ b" "caW" "a |b"

    module iw =
        // Tests for iw. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#iw
        [<Test>]
        let ``diw deletes inside word``() =
            assertColemakText "a b$c d" "suw" "a  $d"

        [<Test>]
        let ``diw on sequence of other characters``() =
            assertColemakText "a ~!@#%^&*=+:;?/<>(){}$b " "suw" "a b$ "

        [<Test>]
        let ``ciw changes a word``() =
            assertColemakText "a b$ c" "cuw" "a | c"

        [<Test>]
        let ``diw stops at non-word character``() =
            assertColemakText "%b$_123." "suw" "%.$"

        [<Test>]
        let ``diw deletes whitespace``() =
            assertColemakText "a   $   b" "suw" "ab$"

        [<Test>]
        let ``diw does not extend to next line``() =
            assertColemakText "a $ \n b" "suw" "a$\n b"

        [<Test>]
        let ``ciw changes inside word``() =
            assertColemakText "a b$c d" "cuw" "a | d"

        // Tests for iW.
        [<Test>]
        let ``diW deletes a WORD``() =
            assertColemakText "a W.W$ b" "suW" "a  $b"

        [<Test>]
        let ``ciW changes a WORD``() =
            assertColemakText "a W.W2$ b" "cuW" "a | b"

    // Tests for quoted strings. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#a`
    // Handling of different quotes is identical. The tests alternate between ', " and `
    [<Test>]
    let ``ci' before quoted string``()  =
        assertColemakText "var$ a = 'value'" "cu'" "var a = '|'"

    [<Test>]
    let ``ci" inside quoted string``() =
        assertColemakText "var a = \"value$\"" "cu\"" "var a = \"|\""

    [<Test>]
    let ``di` before quoted string``() =
        assertColemakText "var$ a = `value`" "su`" "var a = ``$"

    [<Test>]
    let ``di' inside quoted string``() =
        assertColemakText "var a = 'value$'" "su'" "var a = ''$"

    // TODO: ca" and da" tests cheat. The commands should delete the white space after the closing quote
    [<Test>]
    let ``ca" before quoted string``()  =
        assertColemakText "var$ a = \"value\"" "ca\"" "var a = |"

    [<Test>]
    let ``ca` inside quoted string``() =
        assertColemakText "var a = `value$`" "ca`" "var a = |"

    [<Test>]
    let ``da' before quoted string``() =
        assertColemakText "var$ a = 'value'" "sa'" "var a = $"

    [<Test>]
    let ``da` inside quoted string``() =
        assertColemakText "var a = `value$`" "sa`" "var a = $"

    [<Test>]
    [<Ignore "Didn't find out how to signal NoOp from getRange">]
    let ``ci" does nothing when no quoted text on line``() =
        assertColemakText "var$ a = b\n" "cu\"" "var$ a = b\n"

    [<Test>]
    [<Ignore "Handling of escaped quotes is not implemented">]
    let ``ci" handles escaped quote``() =
        assertColemakText """ var$ a = "\"" """ "cu\"" " var a = \"$\" "

    // Tests for braces. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#a)
    [<Test>]
    let ``ci( handles nested parentheses backwards``() =
        assertColemakText "(ignored)\n((abc)\n de$f)\n(ignored)" "cu(" "(ignored)\n(|)\n(ignored)"

    [<Test>]
    let ``ci( handles nested parentheses forwards``() =
        assertColemakText "(ignored)\n(abc$\n (def))\n(ignored)" "cu(" "(ignored)\n(|)\n(ignored)"

    [<Test>]
    let ``da{ handles nested braces forwards``() =
        assertColemakText "{a$ {b}}\n{ignored}" "sa{" "\n${ignored}"

    // Tags. Reeference: http://vimdoc.sourceforge.net/htmldoc/motion.html#tag-blocks
    module at =
        [<Test>]
        let ``dat from tag content``() =
            assertColemakText "<a><b>val$</b></a>" "sag" "<a><$/a>"

        [<Test>]
        let ``dat from tag declaration``() =
            assertColemakText "<a><b$>val</b></a>" "sag" "<a><$/a>"

        [<Test>]
        let ``dat ignores unclosed tag``() =
            assertColemakText "<a$>val" "sag" "<a$>val"

        [<Test>]
        let ``dat ignores self-closing tag``() =
            assertColemakText "<a><b$/></a>" "sag" "$"

        [<Test>]
        let ``dat handles tags with attributes``() =
            assertColemakText "<a><b atr='val'>val$</b></a>" "sag" "<a><$/a>"

        [<Test>]
        let ``dat handles tags with namespaces``() =
            assertColemakText "<a><ns:b>val$</ns:b></a>" "sag" "<a><$/a>"

        [<Test>]
        let ``dat finds unmatched closing tag``() =
            assertColemakText "<a><b$><b></b></b></a>" "sag" "<a><$/a>"

        [<Test>]
        let ``dat when closing > is on separate line``() =
            assertColemakText "<a\n>val$</a>" "sag" "$"

        [<Test>]
        let ``cat from tag content``() =
            assertColemakText "<a><b>val$</b></a>" "cag" "<a>|</a>"

        [<Test>]
        let ``cat from tag declaration``() =
            assertColemakText "<a><b$>val</b></a>" "cag" "<a>|</a>"

        [<Test>]
        [<Ignore("Need to find out how to signal NoOp from getRange")>]
        let ``cat ignores unclosed tag``() =
            assertColemakText "<a$>val" "cag" "<a$>val"

        [<Test>]
        let ``cat handles tags with attributes``() =
            assertColemakText "<a><b atr='val'>val$</b></a>" "cag" "<a>|</a>"

        [<Test>]
        let ``cat ignores self-closing tag``() =
            assertColemakText "<a><b$/></a>" "cag" "|"

    module it =
        [<Test>]
        let ``dit from tag content``() =
            assertColemakText "<a><b>val$</b></a>" "sug" "<a><b><$/b></a>"

        [<Test>]
        let ``dit from tag declaration``() =
            assertColemakText "<a><b$>val</b></a>" "sug" "<a><b><$/b></a>"

        [<Test>]
        let ``dit finds unmatched tag``() =
            assertColemakText "<a><b$><b></b></b></a>" "sug" "<a><b><$/b></a>"

        [<Test>]
        let ``dit outside a tag does nothing``() =
            assertColemakText " $ <a>val</a>" "sug" " $ <a>val</a>"

        [<Test>]
        let ``cit from tag content``() =
            assertColemakText "<a><b>val$</b></a>" "cug" "<a><b>|</b></a>"

        [<Test>]
        let ``cit from tag declaration``() =
            assertColemakText "<a><b$>val</b></a>" "cug" "<a><b>|</b></a>"

        [<Test>]
        let ``cit finds unmatched tag``() =
            assertColemakText "<a><b$><b></b></b></a>" "cug" "<a><b>|</b></a>"

        [<Test>]
        let ``cit when closing > is on separate line``() =
            assertColemakText "<a\n>val$</a>" "cug" "<a\n>|</a>"

        [<Test>]
        [<Ignore("Need to find out how to signal NoOp from getRange")>]
        let ``cit outside a tag does nothing``() =
            assertColemakText " $ <a>val</a>" "cug" " $ <a>val</a>"


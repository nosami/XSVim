namespace XSVim.Tests
open NUnit.Framework
open XSVim
open System.Runtime.CompilerServices
open System.Threading.Tasks

[<TestFixture>]
module ``Text object selection tests`` =
    [<SetUp;AsyncStateMachine(typeof<Task>)>]
    let ``run before tests``() =
        FixtureSetup.initialiseMonoDevelop()

    // Tests for aw. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#aw
    module aw =
        [<Test>]
        let ``daw deletes around word``() =
            assertText "a b$c d" "daw" "a d$"

        [<Test>]
        let ``daw on a word``() =
            assertText "word1   word2$   word3" "daw" "word1   w$ord3" // three spaces before word2 preserved, two after removed

        [<Test>]
        let ``daw from whitespace before a word``() =
            assertText "word1  $  word2   word3" "daw" "word1 $  word3" // four spaces before word2 removed, three after preserved

        [<Test>]
        let ``daw puts caret at next character``() =
            assertText "a b$ ." "daw" "a .$"

        [<Test>]
        let ``daw deletes whitespace only``() =
            assertText "a. b$;" "daw" "a.;$"

        [<Test>]
        let ``daw deletes leading whitespace if there's no trailing``() =
            assertText "a b$" "daw" "a$" //

        [<Test>]
        let ``daw stops searching at EOL``() =
            assertText "word1$    \n word2" "daw" "\n$ word2"

        [<Test>]
        let ``caw on a word``() =
            assertText "word1   word2$  word3" "caw" "word1   |word3"

        [<Test>]
        let ``caw finds end of word``() =
            assertText "a [wo$rd_ ] b" "caw" "a [|] b"

        [<Test>]
        let ``caw on a word with digits and underscores``() =
            assertText ".a_8$ b" "caw" ".|b"

        [<Test>]
        let ``daw on a word delimited by punctuation``() =
            assertText "w1.w2$.w3" "daw" "w1..$w3"

        [<Test>]
        let ``caw on a word delimited by punctuation``() =
            assertText "w1(w2$)w3" "caw" "w1(|)w3"

        [<Test>]
        let ``daw on sequence of other characters``() =
            assertText "a ~!@#%^$&*=+:;?/<>(){}b " "daw" "ab$ "

        [<Test>]
        let ``daw from white space at EOL extends to next line``() =
            assertText "a $ \n b" "daw" "a$"

    module aW =
        // Tests for aW. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#aW
        [<Test>]
        let ``daW deletes a WORD``() =
            assertText "a W.W$ b" "daW" "a b$"

        [<Test>]
        let ``daW removes trailing space``() =
            assertText "a W.W$ " "daW" "a $"

        [<Test>]
        let ``daW removes leading space if no trailing``() =
            assertText "a W.W$" "daW" "a$"

        [<Test>]
        let ``caW changes a WORD``() =
            assertText "a W.W$ b" "caW" "a |b"

    module iw =
        // Tests for iw. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#iw
        [<Test>]
        let ``diw deletes inside word``() =
            assertText "a b$c d" "diw" "a  $d"

        [<Test>]
        let ``diw on sequence of other characters``() =
            assertText "a ~!@#%^&*=+:;?/<>(){}$b " "diw" "a b$ "

        [<Test>]
        let ``ciw changes a word``() =
            assertText "a b$ c" "ciw" "a | c"

        [<Test>]
        let ``diw stops at non-word character``() =
            assertText "%b$_123." "diw" "%.$"

        [<Test>]
        let ``diw deletes whitespace``() =
            assertText "a   $   b" "diw" "ab$"

        [<Test>]
        let ``diw does not extend to next line``() =
            assertText "a $ \n b" "diw" "a$\n b"

        [<Test>]
        let ``ciw changes inside word``() =
            assertText "a b$c d" "ciw" "a | d"

        // Tests for iW.
        [<Test>]
        let ``diW deletes a WORD``() =
            assertText "a W.W$ b" "diW" "a  $b"

        [<Test>]
        let ``ciW changes a WORD``() =
            assertText "a W.W2$ b" "ciW" "a | b"

    // Tests for quoted strings. Reference: http://vimdoc.sourceforge.net/htmldoc/motion.html#a`
    // Handling of different quotes is identical. The tests alternate between ', " and `
    [<Test>]
    let ``ci' before quoted string``()  =
        assertText "var$ a = 'value'" "ci'" "var a = '|'"

    [<Test>]
    let ``ci" inside quoted string``() =
        assertText "var a = \"value$\"" "ci\"" "var a = \"|\""

    [<Test>]
    let ``di` before quoted string``() =
        assertText "var$ a = `value`" "di`" "var a = ``$"

    [<Test>]
    let ``di' inside quoted string``() =
        assertText "var a = 'value$'" "di'" "var a = ''$"

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

    // Tags. Reeference: http://vimdoc.sourceforge.net/htmldoc/motion.html#tag-blocks
    module at =
        [<Test>]
        let ``dat from tag content``() =
            assertText "<a><b>val$</b></a>" "dat" "<a><$/a>"

        [<Test>]
        let ``dat from tag declaration``() =
            assertText "<a><b$>val</b></a>" "dat" "<a><$/a>"

        [<Test>]
        let ``dat ignores unclosed tag``() =
            assertText "<a$>val" "dat" "<a$>val"

        [<Test>]
        let ``dat ignores self-closing tag``() =
            assertText "<a><b$/></a>" "dat" "$"

        [<Test>]
        let ``dat handles tags with attributes``() =
            assertText "<a><b atr='val'>val$</b></a>" "dat" "<a><$/a>"

        [<Test>]
        let ``dat handles tags with namespaces``() =
            assertText "<a><ns:b>val$</ns:b></a>" "dat" "<a><$/a>"

        [<Test>]
        let ``dat finds unmatched closing tag``() =
            assertText "<a><b$><b></b></b></a>" "dat" "<a><$/a>"

        [<Test>]
        let ``dat when closing > is on separate line``() =
            assertText "<a\n>val$</a>" "dat" "$"

        [<Test>]
        let ``cat from tag content``() =
            assertText "<a><b>val$</b></a>" "cat" "<a>|</a>"

        [<Test>]
        let ``cat from tag declaration``() =
            assertText "<a><b$>val</b></a>" "cat" "<a>|</a>"

        [<Test>]
        [<Ignore("Need to find out how to signal NoOp from getRange")>]
        let ``cat ignores unclosed tag``() =
            assertText "<a$>val" "cat" "<a$>val"

        [<Test>]
        let ``cat handles tags with attributes``() =
            assertText "<a><b atr='val'>val$</b></a>" "cat" "<a>|</a>"

        [<Test>]
        let ``cat ignores self-closing tag``() =
            assertText "<a><b$/></a>" "cat" "|"

    module it =
        [<Test>]
        let ``dit from tag content``() =
            assertText "<a><b>val$</b></a>" "dit" "<a><b><$/b></a>"

        [<Test>]
        let ``dit from tag declaration``() =
            assertText "<a><b$>val</b></a>" "dit" "<a><b><$/b></a>"

        [<Test>]
        let ``dit finds unmatched tag``() =
            assertText "<a><b$><b></b></b></a>" "dit" "<a><b><$/b></a>"

        [<Test>]
        let ``dit outside a tag does nothing``() =
            assertText " $ <a>val</a>" "dit" " $ <a>val</a>"

        [<Test>]
        let ``cit from tag content``() =
            assertText "<a><b>val$</b></a>" "cit" "<a><b>|</b></a>"

        [<Test>]
        let ``cit from tag declaration``() =
            assertText "<a><b$>val</b></a>" "cit" "<a><b>|</b></a>"

        [<Test>]
        let ``cit finds unmatched tag``() =
            assertText "<a><b$><b></b></b></a>" "cit" "<a><b>|</b></a>"

        [<Test>]
        let ``cit when closing > is on separate line``() =
            assertText "<a\n>val$</a>" "cit" "<a\n>|</a>"

        [<Test>]
        [<Ignore("Need to find out how to signal NoOp from getRange")>]
        let ``cit outside a tag does nothing``() =
            assertText " $ <a>val</a>" "cit" " $ <a>val</a>"


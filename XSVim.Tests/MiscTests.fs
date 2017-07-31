namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Miscellaneous tests`` =
    [<Test>]
    let ``'A' should put caret at end of the line``() =
        assertText "abc$def\n" "A" "abcdef|\n"

    [<Test>]
    let ``'A' should put caret at EOF``() =
        assertText "abc$def" "A" "abcdef|"

    [<Test>]
    let ``'a' should append after``() =
        assertText "a$bcdef" "a" "a|bcdef"

    [<Test>]
    let ``'a' should append after last char``() =
        assertText "abcdef$\n" "a" "abcdef|\n"

    [<Test>]
    let ``'a' should append before EOF``() =
        assertText "abcdef$" "a" "abcdef|"

    [<Test>]
    let ``'a' on empty line should keep cursor on the current line``() =
        assertText "\n$abc" "a" "|\nabc"

    [<Test>]
    let ``'I' should insert at first non whitespace``() =
        assertText "   abcdef$" "I" "   |abcdef"

    [<Test>]
    let ``Undo repeat``() =
        assertText "a$bc def ghi" "3dwu" "a$bc def ghi"

    [<Test>]
    let ``Repeat typed chars``() =
        assertText " $" "iabc <esc>." "abcabc  $ "

    [<Test>]
    let ``<C-[> escapes``() =
        assertText "    abc$" "i<C-[>" "    ab$c"

    [<Test>]
    let ``Repeat delete word``() =
        assertText "a$bc de fgh" "dww." "de $"

    [<Test>]
    let ``Repeat change word``() =
        assertText "a$bc de fgz " "cwxxx<esc>ww." "xxx de xxx $"

    [<Test>]
    let ``r should be repeatable``() =
        assertText "a$aaa" "rb$." "baab$"

    [<Test>]
    let ``Undo insert mode``() =
        assertText "abc$" "adef ghi jkl<esc>u" "abc$"

    [<Test>]
    let ``J puts caret between joined lines``() =
        assertText "a$bc\ndef" "J" "abc $def"

    [<Test>]
    let ``* finds next word``() =
        assertText " $ abc" "*" "  a$bc"

    [<Test>]
    let ``* does not match substring``() =
        assertText "a$bc abcde abc" "*" "abc abcde a$bc"

    [<Test>]
    let ``* finds next word at caret``() =
        assertText "a$bc abc" "*" "abc a$bc"

    [<Test>]
    let ``* finds next word when on last word char``() =
        assertText "abc$ abc" "*" "abc a$bc"

    [<Test>]
    let ``* wraps to start``() =
        assertText "abc a$bc" "*" "a$bc abc"

    [<Test>]
    let ``* finds next word at EOF``() =
        assertText "abc abc$" "*" "a$bc abc"

    [<Test>]
    let ``# finds previous word at caret``() =
        assertText "abc abc a$bc" "#" "abc a$bc abc"

    [<Test>]
    let ``# matches exact word``() =
        assertText "abc abcde a$bc" "#" "a$bc abcde abc"
    
    [<Test>]
    let ``£ wraps to end``() =
        assertText "a$bc abc" "£" "abc a$bc"

    [<Test>]
    let ``~ toggles case of char at caret``() =
        assertText "a$bc abc" "~" "Ab$c abc"

    [<Test>]
    let ``~ toggles case of selection``() =
        assertText "A$bC abc" "vll~" "a$Bc abc"

    [<Test>]
    let ``<esc> doesn't move caret left onto newline'``() =
        assertText "\nA$bC abc" "o<esc>" "\nAbC abc\n$"

    [<Test>]
    let ``<C-a> increments next number``() =
        assertText "a$bc 9 " "<C-a>" "abc 10$ "

    [<Test>]
    let ``<C-a> increments second number``() =
        assertText "abc 0 $1 " "<C-a>" "abc 0 2$ "

    [<Test>]
    let ``<C-a> increments same width number``() =
        assertText "a$bc 0 " "<C-a>" "abc 1$ "

    [<Test>]
    let ``<C-a> increments number at caret``() =
        assertText "abc 9$ " "<C-a>" "abc 10$ "

    [<Test>]
    let ``<C-a> increments next negative number``() =
        assertText "a$bc -1 " "<C-a>" "abc 0$ "

    [<Test>]
    let ``<C-x> decrements next number``() =
        assertText "a$bc 10 " "<C-x>" "abc 9$ "

    [<Test>]
    let ``<C-x> decrements -1``() =
        assertText "abc -1$ " "<C-x>" "abc -2$ "

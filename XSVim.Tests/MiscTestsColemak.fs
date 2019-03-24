namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Miscellaneous tests Colemak`` =
    [<Test>]
    let ``'A' should put caret at end of the line``() =
        assertColemakText "abc$def\n" "A" "abcdef|\n"

    [<Test>]
    let ``'A' should put caret at EOF``() =
        assertColemakText "abc$def" "A" "abcdef|"

    [<Test>]
    let ``'a' should append after``() =
        assertColemakText "a$bcdef" "a" "a|bcdef"

    [<Test>]
    let ``'a' should append after last char``() =
        assertColemakText "abcdef$\n" "a" "abcdef|\n"

    [<Test>]
    let ``'a' should append before EOF``() =
        assertColemakText "abcdef$" "a" "abcdef|"

    [<Test>]
    let ``'a' on empty line should keep cursor on the current line``() =
        assertColemakText "\n$abc" "a" "|\nabc"

    [<Test>]
    let ``'I' should insert at first non whitespace``() =
        assertColemakText "   abcdef$" "U" "   |abcdef"

    [<Test>]
    let ``Undo repeat``() =
        assertColemakText "a$bc def ghi" "3swl" "a$bc def ghi"

    [<Test>]
    let ``Repeat typed chars``() =
        assertColemakText "d$" "uabc <esc>." "abcabc $ d"

    [<Test>]
    let ``Repeat intellisense``() =
        let _, state, editor =
            test
                """
                System$
                System
                """ "a.Coll"
        // simulate ctrl-space intellisense inserting "Collections" by removing 
        // "Coll" and then inserting "Collections"
        let offset = editor.Text.IndexOf("Coll")
        editor.RemoveText(offset,4) // delete "Coll"
        editor.InsertText(editor.CaretOffset, "Collections")
        let config = { Config.Default with keyboardLayout = Colemak }
        let _, newState, _ = sendKeysToEditor editor "<esc>n." config
        let text = getEditorText editor newState

        text
        |> should equal
                """
                System.Collections
                System.Collections$
                """

    [<Test>]
    let ``backspace is repeated``() =
        assertColemakText "d$" "uabc<bs> <esc>." "abab $ d"

    [<Test; Ignore("TODO")>]
    let ``delete key is repeated``() =
        assertColemakText "d$" "u<del>abc<esc>." "ababc$"

    [<Test>]
    let ``<C-[> escapes``() =
        assertColemakText "    abc$" "u<C-[>" "    ab$c"

    [<Test>]
    let ``Return to normal mode doesn't move past start of line``() =
        assertColemakText "abc\nd$ef" "u<esc>" "abc\nd$ef"

    [<Test>]
    let ``dot repeats at start of line``() =
        assertColemakText 
            """
            def$
            def
            """ 

            "Uabc<esc>n."

            """
            abcdef
            abc$def
            """ 

    [<Test>]
    let ``dot repeats at end of line``() =
        assertColemakText 
            """
            a$bc
            a$bc
            """ 

            "Adef<esc>n."

            """
            abcdef
            abcdef$
            """ 

    [<Test>]
    let ``Repeat delete word``() =
        assertColemakText "a$bc de fgh" "sww." "de $"

    [<Test>]
    let ``Repeat change word``() =
        assertColemakText "a$bc de fgz " "cwxxx<esc>ww." "xxx de xxx$ "

    [<Test>]
    let ``r should be repeatable``() =
        assertColemakText "a$aaa" "pb$." "baab$"

    [<Test>]
    let ``r<ret> inserts <ret> and indents``() =
        assertColemakText "   aaa$\nbbb" "p<ret>" "   aa\n   \n$bbb"

    [<Test>]
    let ``R switches to replace mode``() =
        let _, state, _ = testColemak "a$bc" "P"
        state.mode |> should equal ReplaceMode

    [<Test>]
    let ``R replaces characters``() =
        assertColemakText "a$bc" "PABCD" "ABCD$"

    [<Test>]
    let ``R replaces digits``() =
        assertColemakText "a$bc" "P123" "123$"

    [<Test>]
    let ``Replace mode inserts at end of line``() =
        assertColemakText "a$bc\ndef" "PABCD" "ABCD\n$def"

    [<Test>]
    let ``Replace mode is undoable``() =
        assertColemakText "a$bc\ndef" "PABCD<esc>l" "a$bc\ndef"

    [<Test>]
    let ``Undo insert mode``() =
        assertColemakText "abc$" "adef ghi jkl<esc>l" "abc$"

    [<Test>]
    let ``J puts caret between joined lines``() =
        assertColemakText "a$bc\ndef" "N" "abc $def"

    [<Test>]
    let ``* finds next word``() =
        assertColemakText " $ abc" "*" "  a$bc"

    [<Test>]
    let ``* does not match substring``() =
        assertColemakText "a$bc abcde abc" "*" "abc abcde a$bc"

    [<Test>]
    let ``* finds next word at caret``() =
        assertColemakText "a$bc abc" "*" "abc a$bc"

    [<Test>]
    let ``* finds next word when on last word char``() =
        assertColemakText "abc$ abc" "*" "abc a$bc"

    [<Test>]
    let ``* wraps to start``() =
        assertColemakText "abc a$bc" "*" "a$bc abc"

    [<Test>]
    let ``* finds next word at EOF``() =
        assertColemakText "abc abc$" "*" "a$bc abc"

    [<Test>]
    let ``# finds previous word at caret``() =
        assertColemakText "abc abc a$bc" "#" "abc a$bc abc"

    [<Test>]
    let ``# matches exact word``() =
        assertColemakText "abc abcde a$bc" "#" "a$bc abcde abc"

    [<Test>]
    let ``£ wraps to end``() =
        assertColemakText "a$bc abc" "£" "abc a$bc"

    [<Test>]
    let ``~ toggles case of char at caret``() =
        assertColemakText "a$bc abc" "~" "Ab$c abc"

    [<Test>]
    let ``~ toggles case of selection``() =
        assertColemakText "A$bC abc" "vii~" "a$Bc abc"

    [<Test>]
    let ``<esc> doesn't move caret left onto newline'``() =
        assertColemakText "\nA$bC abc" "y<esc>" "\nAbC abc\n$"

    [<Test>]
    let ``<C-a> increments next number``() =
        assertColemakText "a$bc 9 " "<C-a>" "abc 10$ "

    [<Test>]
    let ``<C-a> increments second number``() =
        assertColemakText "abc 0 $1 " "<C-a>" "abc 0 2$ "

    [<Test>]
    let ``<C-a> increments same width number``() =
        assertColemakText "a$bc 0 " "<C-a>" "abc 1$ "

    [<Test>]
    let ``<C-a> increments number at caret``() =
        assertColemakText "abc 9$ " "<C-a>" "abc 10$ "

    [<Test>]
    let ``<C-a> increments next negative number``() =
        assertColemakText "a$bc -1 " "<C-a>" "abc 0$ "

    [<Test>]
    let ``<C-x> decrements next number``() =
        assertColemakText "a$bc 10 " "<C-x>" "abc 9$ "

    [<Test>]
    let ``<C-x> decrements -1``() =
        assertColemakText "abc -1$ " "<C-x>" "abc -2$ "

    [<Test>]
    let ``dot repeats 2dd``() =
        assertColemakText 
           """
           a$aaaa
           aaaaa
           bbbbb
           bbbbb
           ccccc
           """

           "2ss."

           """
           c$cccc
           """           

    [<Test>]
    let ``dot repeats 2dj``() =
        assertColemakText
            """
            a$aaaa
            aaaaa
            aaaaa
            bbbbb
            bbbbb
            bbbbb
            """

            "2sn."

            """
            $"""

    [<Test>]
    let ``dot repeats 3S``() =
        assertColemakText
            """
            a$aaaa
            aaaaa
            bbbbb
            bbbbb
            ccccc
            ccccc"""

            "3R<esc>."

            """

$            ccccc"""
namespace XSVim.Tests
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Miscellaneous tests Dvorak`` =
    [<Test>]
    let ``'A' should put caret at end of the line``() =
        assertDvorakText "abc$def\n" "A" "abcdef|\n"

    [<Test>]
    let ``'A' should put caret at EOF``() =
        assertDvorakText "abc$def" "A" "abcdef|"

    [<Test>]
    let ``'a' should append after``() =
        assertDvorakText "a$bcdef" "a" "a|bcdef"

    [<Test>]
    let ``'a' should append after last char``() =
        assertDvorakText "abcdef$\n" "a" "abcdef|\n"

    [<Test>]
    let ``'a' should append before EOF``() =
        assertDvorakText "abcdef$" "a" "abcdef|"

    [<Test>]
    let ``'a' on empty line should keep cursor on the current line``() =
        assertDvorakText "\n$abc" "a" "|\nabc"

    [<Test>]
    let ``'I' should insert at first non whitespace``() =
        assertDvorakText "   abcdef$" "C" "   |abcdef"

    [<Test>]
    let ``Undo repeat``() =
        assertDvorakText "a$bc def ghi" "3e,g" "a$bc def ghi"

    [<Test>]
    let ``Repeat typed chars``() =
        assertDvorakText "d$" "cabc <esc>v" "abcabc $ d"

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
        let config = { Config.Default with keyboardLayout = Dvorak }
        let _, newState, _ = sendKeysToEditor editor "<esc>hv" config
        let text = getEditorText editor newState

        text
        |> should equal
                """
                System.Collections
                System.Collections$
                """

    [<Test>]
    let ``backspace is repeated``() =
        assertDvorakText "d$" "cabc<bs> <esc>v" "abab $ d"

    [<Test; Ignore("TODO")>]
    let ``delete key is repeated``() =
        assertDvorakText "d$" "c<del>abc<esc>k" "ababc$"

    [<Test>]
    let ``<C-[> escapes``() =
        assertDvorakText "    abc$" "c<C-[>" "    ab$c"

    [<Test>]
    let ``Return to normal mode doesn't move past start of line``() =
        assertDvorakText "abc\nd$ef" "c<esc>" "abc\nd$ef"

    [<Test>]
    let ``dot repeats at start of line``() =
        assertDvorakText 
            """
            def$
            def
            """ 

            "Cabc<esc>hv"

            """
            abcdef
            abc$def
            """ 

    [<Test>]
    let ``dot repeats at end of line``() =
        assertDvorakText 
            """
            a$bc
            a$bc
            """ 

            "Adef<esc>hv"

            """
            abcdef
            abcdef$
            """ 

    [<Test>]
    let ``Repeat delete word``() =
        assertDvorakText "a$bc de fgh" "e,,v" "de $"

    [<Test>]
    let ``Repeat change word``() = //cwxxx<esc>ww.
        assertDvorakText "a$bc de fgz " "j,xxx<esc>,,v" "xxx de xxx$ "

    [<Test>]
    let ``r should be repeatable``() =
        assertDvorakText "a$aaa" "pb$v" "baab$"

    [<Test>]
    let ``r<ret> inserts <ret> and indents``() =
        assertDvorakText "   aaa$\nbbb" "p<ret>" "   aa\n   \n$bbb"

    [<Test>]
    let ``R switches to replace mode``() =
        let _, state, _ = testDvorak "a$bc" "P"
        state.mode |> should equal ReplaceMode

    [<Test>]
    let ``R replaces characters``() =
        assertDvorakText "a$bc" "PABCD" "ABCD$"

    [<Test>]
    let ``R replaces digits``() =
        assertDvorakText "a$bc" "P123" "123$"

    [<Test>]
    let ``Replace mode inserts at end of line``() =
        assertDvorakText "a$bc\ndef" "PABCD" "ABCD\n$def"

    [<Test>]
    let ``Replace mode is undoable``() =
        assertDvorakText "a$bc\ndef" "PABCD<esc>g" "a$bc\ndef"

    [<Test>]
    let ``Undo insert mode``() =
        assertDvorakText "abc$" "adef ghi jkl<esc>g" "abc$"

    [<Test>]
    let ``J puts caret between joined lines``() =
        assertDvorakText "a$bc\ndef" "H" "abc $def"

    [<Test>]
    let ``* finds next word``() =
        assertDvorakText " $ abc" "*" "  a$bc"

    [<Test>]
    let ``* does not match substring``() =
        assertDvorakText "a$bc abcde abc" "*" "abc abcde a$bc"

    [<Test>]
    let ``* finds next word at caret``() =
        assertDvorakText "a$bc abc" "*" "abc a$bc"

    [<Test>]
    let ``* finds next word when on last word char``() =
        assertDvorakText "abc$ abc" "*" "abc a$bc"

    [<Test>]
    let ``* wraps to start``() =
        assertDvorakText "abc a$bc" "*" "a$bc abc"

    [<Test>]
    let ``* finds next word at EOF``() =
        assertDvorakText "abc abc$" "*" "a$bc abc"

    [<Test>]
    let ``# finds previous word at caret``() =
        assertDvorakText "abc abc a$bc" "#" "abc a$bc abc"

    [<Test>]
    let ``# matches exact word``() =
        assertDvorakText "abc abcde a$bc" "#" "a$bc abcde abc"

    [<Test>]
    let ``£ wraps to end``() =
        assertDvorakText "a$bc abc" "£" "abc a$bc"

    [<Test>]
    let ``~ toggles case of char at caret``() =
        assertDvorakText "a$bc abc" "~" "Ab$c abc"

    [<Test>]
    let ``~ toggles case of selection``() =
        assertDvorakText "A$bC abc" "knn~" "a$Bc abc"

    [<Test>]
    let ``<esc> doesn't move caret left onto newline'``() =
        assertDvorakText "\nA$bC abc" "r<esc>" "\nAbC abc\n$"

    [<Test>]
    let ``<C-a> increments next number``() =
        assertDvorakText "a$bc 9 " "<C-a>" "abc 10$ "

    [<Test>]
    let ``<C-a> increments second number``() =
        assertDvorakText "abc 0 $1 " "<C-a>" "abc 0 2$ "

    [<Test>]
    let ``<C-a> increments same width number``() =
        assertDvorakText "a$bc 0 " "<C-a>" "abc 1$ "

    [<Test>]
    let ``<C-a> increments number at caret``() =
        assertDvorakText "abc 9$ " "<C-a>" "abc 10$ "

    [<Test>]
    let ``<C-a> increments next negative number``() =
        assertDvorakText "a$bc -1 " "<C-a>" "abc 0$ "

    [<Test>]
    let ``<C-x> decrements next number``() =
        assertDvorakText "a$bc 10 " "<C-x>" "abc 9$ "

    [<Test>]
    let ``<C-x> decrements -1``() =
        assertDvorakText "abc -1$ " "<C-x>" "abc -2$ "

    [<Test>]
    let ``dot repeats 2dd``() =
        assertDvorakText 
           """
           a$aaaa
           aaaaa
           bbbbb
           bbbbb
           ccccc
           """

           "2eev"

           """
           c$cccc
           """           

    [<Test>]
    let ``dot repeats 2dj``() =
        assertDvorakText
            """
            a$aaaa
            aaaaa
            aaaaa
            bbbbb
            bbbbb
            bbbbb
            """

            "2ehv"

            """
            $"""

    [<Test>]
    let ``dot repeats 3S``() =
        assertDvorakText
            """
            a$aaaa
            aaaaa
            bbbbb
            bbbbb
            ccccc
            ccccc"""

            "3O<esc>v"

            """

$            ccccc"""
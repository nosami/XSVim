﻿namespace XSVim.Tests

open XSVim
open NUnit.Framework
open XSVim

[<TestFixture>]
module ``Yank and put tests`` =
    [<Test>]
    let ``Yanking puts cursor at original position before selection was made``() =
        assertText "a$bc" "vly" "a$bc"

    [<Test>]
    let ``Yanking line supports multiplier``() =
        let _, state = test "a$bc\ndef\nghi" "2yy"
        Vim.registers.[EmptyRegister] |> should equal "abc\ndef\n"

    [<Test>]
    let ``Yanking doesn't move caret when there is no selection'``() =
        assertText "a$bcdef" "vll<esc>y$" "abc$def"

    [<Test>]
    let ``Should put line at last line``() =
        assertText "  abc$\ndef" "yyjp" "  abc\ndef\n  a$bc"

    [<Test>]
    let ``Should put ab after``() =
        assertText "a$bc" "vldp" "cab$"

    [<Test>]
    let ``Should put abc over selection in visual mode``() =
        assertText "a$bc" "vlyvllp" "ab$"

    [<Test>]
    let ``P acts like p in visual mode``() =
        assertText "a$bc" "vlyvllP" "ab$"

    [<Test>]
    let ``Can yank into a named register``() =
        let _, state = test "ab$cd ef" "\"dyl"
        System.Console.WriteLine(Vim.registers.Keys.Count)
        Vim.registers.[Register 'd'] |> should equal "b"

    [<Test>]
    [<Ignore>]
    //Todo: fix this and remove ignore attribute.
    let ``yw at the end of a line consumes entire line``()=
        assertText "a$bc" "ywp" "aabc$bc"

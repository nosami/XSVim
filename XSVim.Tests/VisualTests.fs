namespace XSVim.Tests
open NUnit.Framework

[<TestFixture>]
module ``Visual tests`` =
    [<Test>]
    let ``Visual to end of line``() =
        let _, state = test "abc$ def\nghi" "v$y"
        state.clipboard |> should equal "c def"

    [<Test>]
    let ``Visual to end of word``() =
        let _, state = test "ab$c def\nghi" "vey"
        state.clipboard |> should equal "bc"

    [<Test>]
    let ``Visual to d``() =
        let _, state = test "ab$cdef" "vtdy"
        state.clipboard |> should equal "bc"

    [<Test>]
    let ``Visual to d inclusive``() =
        let _, state = test "ab$cdef" "vfdy"
        state.clipboard |> should equal "bcd"

    [<Test>]
    let ``Visual line``() =
        let _, state = test "aaa\nbb$b\nddd" "Vy"
        state.clipboard |> should equal "bbb\n"
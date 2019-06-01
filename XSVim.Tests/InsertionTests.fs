namespace XSVim.Tests
open NUnit.Framework
open XSVim
open System.Runtime.CompilerServices
open System.Threading.Tasks

[<TestFixture>]
module ``Insertion tests`` =
    [<SetUp;AsyncStateMachine(typeof<Task>)>]
    let ``run before tests``() =
        FixtureSetup.initialiseMonoDevelop()

    [<Test>]
    let ``'O' should insert line above``() =
        //TODO: 'O' is broken on the top line
        assertText " \n a$bcdef" "O" " \n |\n abcdef"

    [<Test>]
    let ``'O' inserts line above``() =
        assertText "abc$def\n" "O" "|\nabcdef\n"

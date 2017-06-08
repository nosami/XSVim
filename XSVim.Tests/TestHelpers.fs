namespace XSVim.Tests

open System
open MonoDevelop.Ide.Editor.Extension
open NUnit.Framework
open XSVim
open MonoDevelop.Core
open MonoDevelop.Ide.Editor
[<AutoOpen>]
module FsUnit =

    open System.Diagnostics
    open NUnit.Framework
    open NUnit.Framework.Constraints

    [<DebuggerNonUserCode>]
    let should (f : 'a -> #Constraint) x (y : obj) =
        let c = f x
        let y =
            match y with
            | :? (unit -> unit) -> box (new TestDelegate(y :?> unit -> unit))
            | _                 -> y
        Assert.That(y, c)

    let shouldnot (f : 'a -> #Constraint) x (y : obj) =
        let c = f x
        let y =
            match y with
            | :? (unit -> unit) -> box (new TestDelegate(y :?> unit -> unit))
            | _                 -> y
        Assert.That(y, new NotConstraint(c))

    let equal x = new EqualConstraint(x)

    // like "should equal", but validates same-type
    let shouldEqual (x: 'a) (y: 'a) = Assert.AreEqual(x, y, sprintf "Expected: %A\nActual: %A" x y)
    let replaceLineEnding (s:string) =
      s.Replace("\r\n", "\n")

    let shouldEqualIgnoringLineEndings (x: string) (y: string) =
      Assert.AreEqual((replaceLineEnding x), (replaceLineEnding y), sprintf "Expected: %A\nActual: %A" x y)

    let notEqual x = new NotConstraint(new EqualConstraint(x))

    let NOT c = new NotConstraint(c)

    let contain x = new ContainsConstraint(x)

    let haveLength n = Has.Length.EqualTo(n)

    let haveCount n = Has.Count.EqualTo(n)

    let NotEmpty = Has.Length.GreaterThan(0)

    let endWith (s:string) = new EndsWithConstraint(s)

    let startWith (s:string) = new StartsWithConstraint(s)

    let be = id

    let Null = new NullConstraint()

    let Empty = new EmptyConstraint()

    let EmptyString = new EmptyStringConstraint()

    let NullOrEmptyString = new NullOrEmptyStringConstraint()

    let True = new TrueConstraint()

    let False = new FalseConstraint()

    let sameAs x = new SameAsConstraint(x)

    let throw = Throws.TypeOf

module FixtureSetup =
    let firstRun = ref true

    let initialiseMonoDevelop() =
        if !firstRun then
            printf "initialising"
            firstRun := false
            Environment.SetEnvironmentVariable ("MONO_ADDINS_REGISTRY", "/tmp")
            //Environment.SetEnvironmentVariable ("XDG_CONFIG_HOME", "/tmp")
            Runtime.Initialize (true)
            MonoDevelop.Ide.DesktopService.Initialize()

[<AutoOpen>]
module TestHelpers =
    let (|CtrlKey|_|) (s:string) =
        match s with
        | s when s.Length = 3 && s.StartsWith "C-" -> Some s.[2]
        | _ -> None

    let groupToKeys = function
        | "esc" -> [| KeyDescriptor.FromGtk(Gdk.Key.Escape, '\000', Gdk.ModifierType.None) |]
        | CtrlKey ch -> [| KeyDescriptor.FromGtk(Gdk.Key.a (* important? *), ch, Gdk.ModifierType.ControlMask) |]
        | keys ->
            keys.ToCharArray() 
            |> Array.map (fun c -> KeyDescriptor.FromGtk(Gdk.Key.a (* important? *), c, Gdk.ModifierType.None))

    let parseKeys (keys:string) =
        let delimChars = [|'<'; '>'|]
        let groups = keys.Split delimChars
        groups |> Array.collect groupToKeys
          
    let test (source:string) (keys:string) =
        FixtureSetup.initialiseMonoDevelop()
        let editor = TextEditorFactory.CreateNewEditor()
        let caret = source.IndexOf "$"
        if caret = 0 then
            failwith "$ can't be the first position. It needs to be after some char."
        if caret = -1 then
            failwith "No caret found in test code"
        editor.Text <- source.Replace("$", "")
        editor.CaretOffset <- caret-1

        let plugin = new XSVim()
        let state = Vim.defaultState
        let keyDescriptors = parseKeys keys
        let newState =
            keyDescriptors
            |> Array.fold(fun state c ->
                let newState, handledKeyPress = Vim.handleKeyPress state c editor
                printfn "%A" newState
                printfn "%s" editor.Text
                if state.mode = InsertMode && c.SpecialKey <> SpecialKey.Escape then
                    editor.InsertAtCaret (c.KeyChar.ToString())
                newState) state

        let cursor = if newState.mode = InsertMode then "|" else "$"
        let text =
            if newState.mode = InsertMode then
                editor.Text.Insert(editor.CaretOffset, "|")
            else
                if editor.CaretOffset = editor.Text.Length then
                    editor.Text + "$"
                else
                    editor.Text.Insert(editor.CaretOffset+1, "$")
        text, newState

    let assertText (source:string) (keys:string) expected =
        let actual, _ = test source keys
        Assert.AreEqual(expected, actual)

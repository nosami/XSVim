namespace XSVim.Tests

open System
open System.Text.RegularExpressions
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
        | "ret" -> [| KeyDescriptor.FromGtk(Gdk.Key.Return, '\000', Gdk.ModifierType.None) |]
        | "bs" -> [| KeyDescriptor.FromGtk(Gdk.Key.BackSpace, '\000', Gdk.ModifierType.None) |]
        | CtrlKey ch -> [| KeyDescriptor.FromGtk(Gdk.Key.a (* important? *), ch, Gdk.ModifierType.ControlMask) |]
        | keys ->
            keys.ToCharArray()
            |> Array.map (fun c -> KeyDescriptor.FromGtk(Gdk.Key.a (* important? *), c, Gdk.ModifierType.None))

    let parseKeys (keys:string) =
        let keys = Regex.Replace(keys, "<(.*?)>", "§$1§")
        keys.Split '§' |> Array.collect groupToKeys

    let createEditor (source:string) = 
        FixtureSetup.initialiseMonoDevelop()
        let editor = TextEditorFactory.CreateNewEditor()
        let caret = source.IndexOf "$"
        if caret = 0 then
            failwith "$ can't be the first position. It needs to be after the char the caret would appear over."
        if caret = -1 then
            failwith "No caret found in test code"
        editor.Text <- source.Replace("$", "")
        editor.CaretOffset <- caret-1
        editor

    let processKeys editor keys initialState =
        let config = { insertModeEscapeKey = None }
        let keyDescriptors = parseKeys keys
        keyDescriptors
        |> Array.fold(fun state c ->
            let handledState, handledKeyPress = Vim.handleKeyPress state c editor config
            printfn "%A" handledState
            printfn "%s" editor.Text
            if state.mode = InsertMode && c.ModifierKeys <> ModifierKeys.Control && c.SpecialKey <> SpecialKey.Escape then
                editor.InsertAtCaret (c.KeyChar.ToString())
            handledState) initialState

    let testWithEol (source:string) (keys:string) eolMarker =
        let editor = createEditor source
        editor.Options <- new CustomEditorOptions(TabsToSpaces=true, IndentationSize=4, IndentStyle=IndentStyle.Smart, TabSize=4, DefaultEolMarker=eolMarker)
        let state = processKeys editor keys VimState.Default
        let cursor = if state.mode = InsertMode then "|" else "$"
        let text =
            if state.mode = InsertMode then
                editor.Text.Insert(editor.CaretOffset, "|")
            else
                if editor.CaretOffset = editor.Text.Length then
                    editor.Text + "$"
                else
                    editor.Text.Insert(editor.CaretOffset+1, "$")
        text, state

    let test source keys = testWithEol source keys "\n"

    let switchLineEndings (s:string) =
        s.Replace("\n", "\r\n")

    let assertText (source:string) (keys:string) expected =
        let actual, _ = test source keys
        Assert.AreEqual(expected, actual, "Failed with \n")
        if source.Contains("\n") || actual.Contains("\n") then
            // Run the test again with \r\n line endings
            let actual, _ = testWithEol (source |> switchLineEndings) keys "\r\n"
            Assert.AreEqual(expected |> switchLineEndings, actual.Replace("\r$\n", "\r\n$"), "Failed with \r\n")

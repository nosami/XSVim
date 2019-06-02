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
        async {
            if !firstRun then
                firstRun := false
                printf "initialising"
                Environment.SetEnvironmentVariable ("MONO_ADDINS_REGISTRY", "/tmp")
                Runtime.Initialize (true)
                // Initialize FontService
                let task = Runtime.ServiceProvider.GetService<MonoDevelop.Ide.Fonts.FontService>()
                let! _ = task |> Async.AwaitTask
                ()
        } |> Async.StartImmediateAsTask

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
        | "del" -> [| KeyDescriptor.FromGtk(Gdk.Key.Delete, '\000', Gdk.ModifierType.None) |]
        | CtrlKey ch -> [| KeyDescriptor.FromGtk(Gdk.Key.a (* important? *), ch, Gdk.ModifierType.ControlMask) |]
        | keys ->
            keys.ToCharArray()
            |> Array.map (fun c -> KeyDescriptor.FromGtk(Gdk.Key.a (* important? *), c, Gdk.ModifierType.None))

    let groupToKeys2 keys mode layout =
        match keys, mode with
        | "esc", _  -> [| KeyDescriptor.FromGtk(Gdk.Key.Escape, '\000', Gdk.ModifierType.None) |]
        | "ret", _  -> [| KeyDescriptor.FromGtk(Gdk.Key.Return, '\000', Gdk.ModifierType.None) |]
        | "bs" , _ -> [| KeyDescriptor.FromGtk(Gdk.Key.BackSpace, '\000', Gdk.ModifierType.None) |]
        | "del", _  -> [| KeyDescriptor.FromGtk(Gdk.Key.Delete, '\000', Gdk.ModifierType.None) |]
        | CtrlKey ch, _ -> [| KeyDescriptor.FromGtk(Gdk.Key.a (* important? *), ch, Gdk.ModifierType.ControlMask) |]
        | keys, NormalMode -> 
            keys.ToCharArray()
            |> Array.map (fun c ->
                KeyDescriptor.FromGtk(Gdk.Key.a (* important? *), mapFromQwerty.remap layout c, Gdk.ModifierType.None))
        | keys, _ ->
            keys.ToCharArray()
            |> Array.map (fun c ->
                KeyDescriptor.FromGtk(Gdk.Key.a (* important? *), c, Gdk.ModifierType.None))

    let keyToDescriptor key (state: VimState) layout =
        let lastKeyPress = state.keys |> List.tryLast

        let noremap = // The next key press is not translated for keyboard layout
            [ 'm'; '@'; 'r'; 'f'; 'q']
            |> List.map (mapFromQwerty.remap layout)
            |> List.map Key
            |> List.map Some

        match key, state with
        | "esc", _  -> KeyDescriptor.FromGtk(Gdk.Key.Escape, '\000', Gdk.ModifierType.None)
        | "ret", _  -> KeyDescriptor.FromGtk(Gdk.Key.Return, '\000', Gdk.ModifierType.None)
        | "bs" , _ -> KeyDescriptor.FromGtk(Gdk.Key.BackSpace, '\000', Gdk.ModifierType.None)
        | "del", _  -> KeyDescriptor.FromGtk(Gdk.Key.Delete, '\000', Gdk.ModifierType.None)
        | CtrlKey ch, _ -> KeyDescriptor.FromGtk(Gdk.Key.a (* important? *), ch, Gdk.ModifierType.ControlMask)
        | key, _ when noremap |> List.contains(lastKeyPress) ->
            KeyDescriptor.FromGtk(Gdk.Key.a (* important? *), char key, Gdk.ModifierType.None)
        | key, { mode = NormalMode }
        | key, { mode = VisualMode }
        | key, { mode = VisualLineMode }
        | key, { mode = VisualBlockMode } ->
            KeyDescriptor.FromGtk(Gdk.Key.a (* important? *), mapFromQwerty.remap layout (char key), Gdk.ModifierType.None)
        | key, _ ->
            KeyDescriptor.FromGtk(Gdk.Key.a (* important? *), char key, Gdk.ModifierType.None)
    let parseKeys (keys:string) =
        let keys = Regex.Replace(keys, "<(.*?)>", "§$1§")
        //let g = groupToKeys
        keys.Split '§' |> Array.collect groupToKeys

    let getEditorText (editor:TextEditor) state =
        if state.mode = InsertMode then
            editor.Text.Insert(editor.CaretOffset, "|")
        else
            if editor.CaretOffset = editor.Text.Length then
                editor.Text + "$"
            else
                editor.Text.Insert(editor.CaretOffset+1, "$")

    let sendKeysToEditor (editor:TextEditor) keys config =
        let keyDescriptors = parseKeys keys
        let newState =
            keyDescriptors
            |> Array.fold(fun state descriptor ->
                let state = Vim.editorStates.[editor.FileName]
                let handledState, handledKeyPress = Vim.handleKeyPress state descriptor editor config
                printfn "%A" handledState
                printfn "\"%s\"" (getEditorText editor handledState)
                if state.mode = InsertMode && descriptor.ModifierKeys <> ModifierKeys.Control && descriptor.SpecialKey <> SpecialKey.Escape then
                    Vim.processVimKey editor (Vim.keyPressToVimKey descriptor)
                handledState) Vim.editorStates.[editor.FileName]

        let text = getEditorText editor newState
        text, newState, editor

    let sendKeysToEditor2 (editor:TextEditor) keys config =
        let keygroups =
            Regex.Replace(keys, "<(.*?)>", "§$1§").Split '§'

        let keygroups =
            keygroups
            |> Array.collect(fun g ->
                                 match g with
                                 | "esc" -> [|g|]
                                 | "ret" -> [|g|]
                                 | "bs"  -> [|g|]
                                 | "del" -> [|g|]
                                 | CtrlKey ch -> [|g|]
                                 | _ ->  Seq.toArray g |> Array.map string) // "abc" -> [|"a"; "b"; "c" |]

        let mutable s:string = String.Empty

        let newState =
            keygroups
            |> Array.fold(fun state keys ->
                let state = Vim.editorStates.[editor.FileName]

                printfn "%A" state.keys
                let descriptor = keyToDescriptor keys state config.keyboardLayout
                s <- s + descriptor.KeyChar.ToString()
                printfn "%A" state.mode
                printfn "%A" descriptor

                let handledState, handledKeyPress = Vim.handleKeyPress state descriptor editor config
                printfn "%A" handledState
                printfn "\"%s\"" (getEditorText editor handledState)
                if state.mode = InsertMode && descriptor.ModifierKeys <> ModifierKeys.Control && descriptor.SpecialKey <> SpecialKey.Escape then
                    Vim.processVimKey editor (Vim.keyPressToVimKey descriptor)
                handledState) Vim.editorStates.[editor.FileName]

        printfn "remapped keys %s" s
        let text = getEditorText editor newState
        text, newState, editor

    let testWithEol (source:string) (keys:string) eolMarker layout =
        let config = { Config.Default with keyboardLayout = layout }
        let editor = TextEditorFactory.CreateNewEditor()
        editor.FileName <- FilePath "test.txt"
        editor.TextChanged.Add(fun changes -> Subscriptions.textChanged editor changes)
        let caret = source.IndexOf "$"
        if caret = 0 then
            failwith "$ can't be the first position. It needs to be after the char the caret would appear over."
        if caret = -1 then
            failwith "No caret found in test code"
        editor.Text <- source.Replace("$", "")
        editor.CaretOffset <- caret-1
        editor.Options <- new CustomEditorOptions(TabsToSpaces=true, IndentationSize=4, IndentStyle=IndentStyle.Smart, TabSize=4, DefaultEolMarker=eolMarker)
        Vim.editorStates.[editor.FileName] <- VimState.Default
        sendKeysToEditor2 editor keys config

    let test source keys = testWithEol source keys "\n" Qwerty

    let switchLineEndings (s:string) =
        s.Replace("\n", "\r\n")

    let assertText (source:string) (keys:string) expected =
        let actual, _, _ = testWithEol source keys "\n" Qwerty
        Assert.AreEqual(expected, actual, "Failed with \n")
        let actual, _, _ = testWithEol source keys "\n" Colemak
        Assert.AreEqual(expected, actual, "Failed with colemak")
        let actual, _, _ = testWithEol source keys "\n" Dvorak
        Assert.AreEqual(expected, actual, "Failed with dvorak")
        if source.Contains("\n") || actual.Contains("\n") then
            // Run the test again with \r\n line endings
            let actual, _, _ = testWithEol (source |> switchLineEndings) keys "\r\n" Qwerty
            Assert.AreEqual(expected |> switchLineEndings, actual.Replace("\r$\n", "\r\n$"), "Failed with \r\n")

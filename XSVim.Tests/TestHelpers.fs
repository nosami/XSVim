namespace XSVim.Tests

open System
open MonoDevelop.Ide.Editor.Extension
open NUnit.Framework
open XSVim
open MonoDevelop.Core
open MonoDevelop.Ide.Editor

module FixtureSetup =
    let firstRun = ref true

    let initialiseMonoDevelop() =
        if !firstRun then
            firstRun := false
            Environment.SetEnvironmentVariable ("MONO_ADDINS_REGISTRY", "/tmp")
            //Environment.SetEnvironmentVariable ("XDG_CONFIG_HOME", "/tmp")
            //MonoDevelop.FSharp.MDLanguageService.DisableVirtualFileSystem()
            Runtime.Initialize (true)
            MonoDevelop.Ide.DesktopService.Initialize()

            //GuiUnit.TestRunner.ExitCode |> ignore // hack to get GuiUnit into the AppDomain

[<AutoOpen>]
module TestHelpers =
    let test (source:string) (keys:string) expected =
        FixtureSetup.initialiseMonoDevelop()
        let editor = TextEditorFactory.CreateNewEditor()
        let caret = source.IndexOf "$"
        editor.Text <- source.Replace("$", "")
        editor.CaretOffset <- caret-1
        //editor.Caret.UpdateCaretOffset()
        let plugin = new XSVim()
        let state = { keys=[]; mode=NormalMode; visualStartOffset=0; findCharCommand=None; lastAction=[]; clipboard="" }
        let newState =
            keys |> Seq.fold(fun state c ->
                let descriptor = KeyDescriptor.FromGtk(Gdk.Key.a (* important? *), c, Gdk.ModifierType.None)
                let newState, handledKeyPress = Vim.handleKeyPress state descriptor editor
                newState) state

        let cursor = if newState.mode = InsertMode then "|" else "$"
        let actual = editor.Text.Insert(editor.CaretOffset+1, cursor)
        Assert.AreEqual(expected, actual)


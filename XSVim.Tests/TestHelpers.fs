namespace XSVim.Tests

open System
open System.Text
open Mono.TextEditor
open MonoDevelop.Ide.Editor.Extension
open NUnit.Framework
open XSVim

[<AutoOpen>]
module TestHelpers =
    let test (source:string) (keys:string) expected expectedMode =
        let editor = new TextEditorData()
        let caret = source.IndexOf "$"
        editor.Text <- source.Replace("$", "")
        editor.Caret.Offset <- caret
        editor.Caret.UpdateCaretOffset()
        let plugin = new XSVim()
        let state = { keys=[]; mode=NormalMode; visualStartOffset=0; findCharCommand=None; lastAction=[] }
        let newState =
            keys |> Seq.fold(fun state c ->
                let descriptor = KeyDescriptor.FromGtk(Gdk.Key.a (* important? *), c, Gdk.ModifierType.None)
                let newState, handledKeyPress = Vim.handleKeyPress state descriptor editor
                newState) state

        Assert.AreEqual(expected, editor.Text)


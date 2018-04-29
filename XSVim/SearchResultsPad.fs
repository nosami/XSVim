namespace XSVim
open System
open Gtk
open MonoDevelop.Ide.FindInFiles
open MonoDevelop.Ide.Gui.Components
open Reflection

module searchResultsPad =
    let select (tree:TreeView) path =
        let column = tree.Columns.[0]
        tree.Selection.SelectPath path
        tree.SetCursor(path, column, false)

    let getSelectedPath (tree:TreeView) =
        tree.Selection.GetSelectedRows().[0]

    let moveDown tree =
        let path = getSelectedPath tree
        path.Next()
        select tree path

    let moveUp tree =
        let path = getSelectedPath tree
        path.Prev() |> ignore
        select tree path

    let initialize (pad:SearchResultPad) =
        let tree:PadTreeView = pad.Control?nativeWidget?treeviewSearchResults
        let processKey (key:KeyPressEventArgs) =
            match key.Event.Key with
            | Gdk.Key.Escape ->
                dispatchCommand "MonoDevelop.Ide.Commands.ViewCommands.FocusCurrentDocument"
                key.RetVal <- false
            | Gdk.Key.j ->
                moveDown tree
                key.RetVal <- true
            | Gdk.Key.k ->
                moveUp tree
                key.RetVal <- true
            | _ -> ()

        tree.KeyPressEvent.Add processKey

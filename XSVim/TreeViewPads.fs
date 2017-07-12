namespace XSVim
open System
open Gtk
open MonoDevelop.Ide
open MonoDevelop.Ide.Gui
open MonoDevelop.Ide.Gui.Components
open MonoDevelop.Ide.Gui.Pads
open Reflection

/// Set up TreeViewPads to accept hjkl keys
module treeViewPads =
    let getTreeViewPads() =
        IdeApp.Workbench.Pads
        |> List.ofSeq
        |> List.map(fun pad -> pad.Content)
        |> List.filter(fun pad -> pad :? TreeViewPad)
        |> Seq.cast<TreeViewPad>

    let select (tree:TreeView) (iter:TreeIter ref) =
        tree.Selection.UnselectAll()
        tree.Selection.SelectIter iter.Value

    let getSelectedNode (pad:TreeViewPad) =
        let (node:ITreeNavigator) = pad.TreeView?GetSelectedNode()
        node

    let getPath (tree:TreeView) =
        tree.Selection.GetSelectedRows().[0]

    let getIter (store:TreeStore) path =
        let iter : TreeIter ref = ref Unchecked.defaultof<_>
        let res = store.GetIter (iter, path)
        res, iter

    let moveDown (tree:TreeView) (store:TreeStore) pad =
        let selected = tree.Selection.GetSelectedRows()
        let getIter = getIter store
        if selected.Length = 1 then
            let path = selected.[0]
            let _res, iterOriginal = getIter path
            path.Down()
            let node = getSelectedNode pad
            let res, iter = getIter path
            if res && node.Expanded then
                select tree iter
            else
                let path = getPath tree
                path.Next()
                let res, iter = getIter path
                if res then
                    select tree iter
                else
                    // parent, then sibling
                    let path = getPath tree
                    let _res = path.Up()
                    let res, iter = getIter path
                    if res then
                        select tree iter
                        let path = getPath tree
                        path.Next()
                        let res, iter = getIter path
                        if res then
                            select tree iter
                        else
                            // must be at the bottom, so select original node
                            select tree iterOriginal

    let moveUp (tree:TreeView) (store:TreeStore) pad =
        let selected = tree.Selection.GetSelectedRows()
        let getIter = getIter store
        if selected.Length = 1 then
            let path = selected.[0]
            let prev = path.Prev()
            let res, iter = getIter path
            if prev && res then
                select tree iter
                let node = getSelectedNode pad
                if node.Expanded then
                    // move to last child
                    let path = getPath tree
                    path.Down()
                    let _res, iter = getIter path
                    let rec moveNext lastIter =
                        path.Next()
                        let res, iter = getIter path
                        if res then
                            moveNext iter
                        else
                            select tree lastIter

                    moveNext iter
            else
                let path = getPath tree
                if path.Depth > 1 then
                    let up = path.Up()
                    let res, iter = getIter path
                    if res && up then
                        select tree iter

    let mutable initialized = false

    /// Set up TreeViewPads to accept hjkl keys
    let initialize() =
        if not initialized then
            initialized <- true

            for pad in getTreeViewPads() do
                let (tree:TreeView) = pad.TreeView?Tree
                let (store:TreeStore) = pad.TreeView?Store

                let processKey (key:KeyPressEventArgs) =
                    match key.Event.Key with
                    | Gdk.Key.Escape ->
                        dispatchCommand "MonoDevelop.Ide.Commands.ViewCommands.FocusCurrentDocument"
                        key.RetVal <- false
                    | Gdk.Key.l ->
                        pad.TreeView?ExpandCurrentItem()
                        key.RetVal <- true
                    | Gdk.Key.h ->
                        pad.TreeView?CollapseCurrentItem()
                        key.RetVal <- true
                    | Gdk.Key.j ->
                        moveDown tree store pad
                        key.RetVal <- true
                    | Gdk.Key.k ->
                        moveUp tree store pad
                        key.RetVal <- true
                    | _ -> ()

                tree.KeyPressEvent.Add processKey

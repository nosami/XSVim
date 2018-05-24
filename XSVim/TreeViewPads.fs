namespace XSVim
open System
open Gtk
open MonoDevelop.Ide
open MonoDevelop.Ide.Gui.Components
open MonoDevelop.Ide.Gui.Pads
open Reflection

/// Set up TreeViewPads to accept hjkl keys
module treeViewPads =
    let getTreeViewPads() =
        match IdeApp.Workbench |> Option.ofObj with
        | Some workbench ->
            workbench.Pads
            |> List.ofSeq
            |> List.choose(fun pad ->
                try
                    // fetching pad.Content can throw when there is an exception
                    // when initializing the pad
                    match pad.Content with
                    | :? TreeViewPad as pad -> Some pad
                    | _ -> None
                with
                | _ -> None)
        | None -> List.empty

    let select (tree:TreeView) path =
        if tree.Selection.GetSelectedRows().Length > 0 then
            tree.Selection.UnselectAll()
        let column = tree.Columns.[0]
        tree.Selection.SelectPath path
        tree.SetCursor(path, column, false)

    let getSelectedNode (pad:TreeViewPad) =
        let (node:ITreeNavigator) = pad.TreeView?GetSelectedNode()
        node

    let getPath (tree:TreeView) =
        tree.Selection.GetSelectedRows().[0]

    let pathExists (store:TreeStore) path =
        let iter : TreeIter ref = ref Unchecked.defaultof<_>
        store.GetIter (iter, path)

    let moveDown (tree:TreeView) (store:TreeStore) pad =
        let selected = tree.Selection.GetSelectedRows()
        let pathExists = pathExists store
        if selected.Length > 0 then
            let path = selected.[0]
            path.Down()
            let node = getSelectedNode pad
            let res = pathExists path
            if res && node.Expanded then
                select tree path
            else
                let path = getPath tree
                path.Next()
                let res = pathExists path
                if res then
                    select tree path
                else
                    // parent, then sibling
                    let path = getPath tree
                    let _res = path.Up()
                    let res = pathExists path
                    if res then
                        path.Next()
                        let res = pathExists path
                        if res then
                            select tree path

    let moveUp (tree:TreeView) (store:TreeStore) pad =
        let selected = tree.Selection.GetSelectedRows()
        let pathExists = pathExists store
        if selected.Length > 0 then
            let path = selected.[0]
            let prev = path.Prev()
            let res = pathExists path
            if prev && res then
                select tree path
                let node = getSelectedNode pad
                if node.Expanded then
                    // move to last child
                    let path = getPath tree
                    path.Down()
                    let rec moveNext lastPath =
                        path.Next()
                        let res = pathExists path
                        if res then
                            moveNext (path.Copy())
                        else
                            select tree lastPath

                    moveNext (path.Copy())
            else
                let path = getPath tree
                if path.Depth > 1 then
                    let up = path.Up()
                    let res = pathExists path
                    if res && up then
                        select tree path

    let mutable initialized = false

    /// Set up TreeViewPads to accept hjkl keys
    let initialize() =
        if not initialized then
            initialized <- true
            let errorPad = IdeApp.Workbench.Pads.ErrorsPad.Content
            let errorPadTree = errorPad?view
            padTreeViews.initialize errorPadTree
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

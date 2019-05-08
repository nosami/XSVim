namespace XSVim
open MonoDevelop.Core
open MonoDevelop.Ide
open MonoDevelop.Ide.Commands
open Reflection

module Window =
    type Notebook = {
        isActive: bool
        activeTab: int
        tabs: string list
    }

    let dispatch command = IdeApp.CommandService.DispatchCommand command |> ignore

    let openDocument fileName =
        let (project:MonoDevelop.Projects.Project) = Unchecked.defaultof<_>
        IdeApp.Workbench.OpenDocument(fileName |> FilePath, project).Wait(System.Threading.CancellationToken.None)

    let switchToNotebook notebook =
        openDocument notebook.tabs.[notebook.activeTab]

    let forceClose() = IdeApp.Workbench.ActiveDocument?Window?CloseWindow true |> Async.AwaitTask |> Async.Ignore

    let getNotebooks() =
        let (dockNotebookContainer: obj seq) = IdeApp.Workbench?RootWindow?TabControl?Container?GetNotebooks()
        let getFiles notebook =
            let tabs = notebook?Tabs
            let tabs' = tabs :> seq<obj>
            tabs' |> Seq.map(fun tab -> tab?Tooltip) |> List.ofSeq

        dockNotebookContainer
        |> Seq.map(fun notebook ->
                       let firstChild = notebook?Children |> Array.tryHead
                       let isActive =
                           firstChild
                           |> Option.map(fun tabstrip -> tabstrip?IsActiveNotebook)
                           |> Option.defaultValue false
                       { isActive=isActive; activeTab=notebook?CurrentTabIndex; tabs=getFiles notebook } )
        |> List.ofSeq

    let tryFindActiveNoteBook() =
        getNotebooks()
        |> List.tryFind(fun notebook -> notebook.isActive)

    let tryActiveInactiveNoteBooks() =
        let notebooks = getNotebooks()
        notebooks |> List.tryFind(fun notebook -> notebook.isActive),
        notebooks |> List.tryFind(fun notebook -> not notebook.isActive)

    let nextTab() =
        tryFindActiveNoteBook() |> Option.iter(fun notebook ->
            let tabCount = notebook.tabs.Length
            let currentTabIndex = notebook.activeTab
            let index =
                if currentTabIndex < (tabCount-1) then
                    currentTabIndex + 1
                else
                    0
            openDocument notebook.tabs.[index])

    let previousTab() =
        tryFindActiveNoteBook() |> Option.iter(fun notebook ->
            let tabCount = notebook.tabs.Length
            let currentTabIndex = notebook.activeTab
            let index =
                if currentTabIndex > 0 then
                    currentTabIndex - 1
                else
                    tabCount - 1
            openDocument notebook.tabs.[index])

    let switchWindow() =
        let notebook =
            getNotebooks()
            |> List.tryFind(fun notebook -> not notebook.isActive)
        notebook |> Option.iter switchToNotebook

    let leftWindow() =
        let notebooks = getNotebooks()
        if notebooks.Length = 2 && notebooks.[1].isActive then
            switchToNotebook notebooks.[0]

    let rightWindow() =
        let notebooks = getNotebooks()
        if notebooks.Length = 2 && notebooks.[0].isActive then
            switchToNotebook notebooks.[1]

    let private closeTabWithForce force  =
        let closeFunc() =
            match force with
            | true -> forceClose() |> Async.RunSynchronously
            | false -> dispatch FileCommands.CloseFile

        match tryActiveInactiveNoteBooks() with
        | Some active, Some inactive when active.tabs.Length = 1 ->
            closeFunc()
            switchToNotebook inactive
        | Some active, _ when active.activeTab > 0 ->
            closeFunc()
            openDocument active.tabs.[active.activeTab-1]
        | Some active, _ when active.activeTab = 0 && active.tabs.Length > 1 ->
            closeFunc()
            openDocument active.tabs.[1]
        | _ -> closeFunc()

    let closeTab() = closeTabWithForce false
    let forceCloseTab() = closeTabWithForce true

    let gotoPad padId =
        IdeApp.Workbench.Pads
        |> Seq.tryFind(fun p -> p.Id = padId)
        |> Option.iter(fun pad -> pad.BringToFront(true))

namespace XSVim
open MonoDevelop.Core
open MonoDevelop.Ide
open MonoDevelop.Ide.Editor
open Reflection

module Window =
    type Notebook = {
        isActive: bool
        activeTab: int
        tabs: string list
    }

    let openDocument fileName =
        let (project:MonoDevelop.Projects.Project) = Unchecked.defaultof<_>
        IdeApp.Workbench.OpenDocument(fileName |> FilePath, project).Wait(System.Threading.CancellationToken.None)

    let switchToNotebook notebook =
        openDocument notebook.tabs.[notebook.activeTab]

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

    let nextTab _editor =
        tryFindActiveNoteBook() |> Option.iter(fun notebook ->
            let tabCount = notebook.tabs.Length
            let currentTabIndex = notebook.activeTab
            let index =
                if currentTabIndex < (tabCount-1) then
                    currentTabIndex + 1
                else
                    0
            openDocument notebook.tabs.[index])

    let previousTab _editor =
        tryFindActiveNoteBook() |> Option.iter(fun notebook ->
            let tabCount = notebook.tabs.Length
            let currentTabIndex = notebook.activeTab
            let index =
                if currentTabIndex > 0 then
                    currentTabIndex - 1
                else
                    tabCount - 1
            openDocument notebook.tabs.[index])

    let switchWindow _editor =
        let notebook =
            getNotebooks()
            |> List.tryFind(fun notebook -> not notebook.isActive)
        notebook |> Option.iter switchToNotebook

    let leftWindow _editor =
        let notebooks = getNotebooks()
        if notebooks.Length = 2 && notebooks.[1].isActive then
            switchToNotebook notebooks.[0]

    let rightWindow _editor =
        let notebooks = getNotebooks()
        if notebooks.Length = 2 && notebooks.[0].isActive then
            switchToNotebook notebooks.[1]

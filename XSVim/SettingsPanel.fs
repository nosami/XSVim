namespace XSVim
open Gtk
open MonoDevelop.Components
open MonoDevelop.Core
open MonoDevelop.Ide.Gui.Dialogs

type SettingsWidget() as this =
    inherit Gtk.Box()

    let labelMapping = new Label "Insert mode escape binding"
    let escapeMappingEntry = new Entry(2)
    let hbox = new HBox(false, 6)

    let vbox = new VBox(true, 6)
    let labelMappingTimeout = new Label "Insert mode mapping timeout (ms)"
    let escapeMappingEntryTimeout = new Entry("1000")
    do
        labelMapping.TooltipText <- "2 character combination to escape from insert mode (jj / hh / jk etc)" 
        labelMappingTimeout.TooltipText <- "Timeout (in milliseconds) before key is registered as an insert mode key press"
        hbox.PackStart labelMapping
        hbox.PackStart escapeMappingEntry
        let hboxTimeout = new HBox(false, 6)
        hboxTimeout.PackStart labelMappingTimeout
        hboxTimeout.PackStart escapeMappingEntryTimeout
        vbox.PackStart hbox
        vbox.PackStart hboxTimeout
        vbox.Add hbox 
        this.Add vbox
        this.ShowAll()

    member this.EscapeMappingEntry = escapeMappingEntry
    member this.EscapeMappingEntryTimeout = escapeMappingEntryTimeout

type SettingsPanel() =
    inherit OptionsPanel()
    let widget = new SettingsWidget()
    static let escapeMappingKey = "VimEscapeMapping"
    static let escapeMappingKeyTimeout = "VimEscapeMappingTimeout"

    static member InsertModeEscapeMapping() =
        PropertyService.Get(escapeMappingKey, "")
    static member InsertModeEscapeMappingTimeout() =
        PropertyService.Get(escapeMappingKeyTimeout, 1000)

    override x.Dispose() = widget.Dispose()

    override x.CreatePanelWidget() =
        widget.EscapeMappingEntry.Text <- SettingsPanel.InsertModeEscapeMapping()
        widget.EscapeMappingEntryTimeout.Text <- SettingsPanel.InsertModeEscapeMappingTimeout() |> string
        widget.Show()
        Control.op_Implicit widget

    override x.ApplyChanges() =
        if widget.EscapeMappingEntry.Text.Length = 2 then
            PropertyService.Set(escapeMappingKey, widget.EscapeMappingEntry.Text)
        else
            let md = new MessageDialog (null, DialogFlags.Modal ||| DialogFlags.DestroyWithParent, MessageType.Error, ButtonsType.Ok, "Timeout must be numeric")
            md.Show()
        PropertyService.Set(escapeMappingKeyTimeout, int widget.EscapeMappingEntryTimeout.Text)

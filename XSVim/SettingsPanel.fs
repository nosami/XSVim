namespace XSVim
open Gtk
open MonoDevelop.Components
open MonoDevelop.Core
open MonoDevelop.Ide.Gui.Dialogs

type SettingsWidget() as this =
    inherit Gtk.Box()

    let labelMapping =
        new Label("Insert mode escape binding",
                  TooltipText = "2 character combination to escape from insert mode (jj / hh / jk etc)")

    let escapeMappingEntry = new Entry(2)
    let hbox = new HBox(false, 6)

    let vbox = new VBox(true, 6)
    let labelMappingTimeout =
        new Label("Insert mode mapping timeout (ms)",
                  TooltipText = "Timeout (in milliseconds) before key is registered as an insert mode key press")

    let escapeMappingEntryTimeout = new Entry("1000")
    let checkDisableAutoCompleteNormalMode =
        new CheckButton("Disable intellisense in Normal mode. Use only if you are having issues with the intellisense drop down.",
                  TooltipText = "Enabling this switches the setting Intellisense on keystroke globally when in Normal mode.")

    let labelKeyboardLayout =
        new Label("Keyboard Layout",
                  TooltipText = "Select an keyboard layout for when not in insert mode")
    let dropDownKeyboardLayout =
        new ComboBox([| "Qwerty"; "Colemak"; "Dvorak" |]);

    do
        hbox.PackStart labelMapping
        hbox.PackStart escapeMappingEntry

        let hboxTimeout = new HBox(false, 6)
        hboxTimeout.PackStart labelMappingTimeout
        hboxTimeout.PackStart escapeMappingEntryTimeout

        let hboxKeyboardLayout = new HBox(false, 6)
        hboxKeyboardLayout.PackStart labelKeyboardLayout
        hboxKeyboardLayout.PackStart dropDownKeyboardLayout

        vbox.PackStart hbox
        vbox.PackStart hboxTimeout
        vbox.PackStart hboxKeyboardLayout
        vbox.Add hbox
        vbox.Add checkDisableAutoCompleteNormalMode
        this.Add vbox
        this.ShowAll()

    member this.EscapeMappingEntry = escapeMappingEntry
    member this.EscapeMappingEntryTimeout = escapeMappingEntryTimeout
    member this.DisableAutoCompleteNormalMode = checkDisableAutoCompleteNormalMode
    member this.KeyboardLayout = dropDownKeyboardLayout

type SettingsPanel() =
    inherit OptionsPanel()
    let widget = new SettingsWidget()
    static let escapeMappingKey = "VimEscapeMapping"
    static let escapeMappingKeyTimeout = "VimEscapeMappingTimeout"
    static let disableAutoComplete = "VimDisableAutoComplete"
    static let keyboardLayout = "VimAlternateMapping"

    static member InsertModeEscapeMapping() =
        PropertyService.Get(escapeMappingKey, "")

    static member InsertModeEscapeMappingTimeout() =
        PropertyService.Get(escapeMappingKeyTimeout, 1000)

    static member AutoCompleteInNormalModeIsDisabled() =
        PropertyService.Get(disableAutoComplete, false)

    static member KeyboardLayout() =
        PropertyService.Get(keyboardLayout, "Qwerty")

    override x.Dispose() = widget.Dispose()

    override x.CreatePanelWidget() =
        widget.EscapeMappingEntry.Text <- SettingsPanel.InsertModeEscapeMapping()
        widget.EscapeMappingEntryTimeout.Text <- SettingsPanel.InsertModeEscapeMappingTimeout() |> string
        widget.DisableAutoCompleteNormalMode.Active <- SettingsPanel.AutoCompleteInNormalModeIsDisabled()
        widget.KeyboardLayout.Active <- match SettingsPanel.KeyboardLayout() with
                                                | "Colemak" -> 1
                                                | "Dvorak" -> 2
                                                | _ -> 0
        widget.Show()
        Control.op_Implicit widget

    override x.ApplyChanges() =
        match widget.EscapeMappingEntry.Text.Length with
        | 2 | 0 ->
            PropertyService.Set(escapeMappingKey, widget.EscapeMappingEntry.Text)
        | _ ->
            let md = new MessageDialog (null, DialogFlags.Modal ||| DialogFlags.DestroyWithParent, MessageType.Error, ButtonsType.Ok, "Mapping must be empty (not used) or 2 characters.")
            md.Show()
        PropertyService.Set(escapeMappingKeyTimeout, int widget.EscapeMappingEntryTimeout.Text)
        PropertyService.Set(disableAutoComplete, widget.DisableAutoCompleteNormalMode.Active)
        PropertyService.Set(keyboardLayout, widget.KeyboardLayout.ActiveText)

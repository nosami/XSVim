namespace XSVim
open MonoDevelop.Core
open MonoDevelop.Core.Text
open MonoDevelop.Ide
open MonoDevelop.Ide.Editor
open MonoDevelop.Ide.Editor.Extension

type Marker(editor:TextEditor, name:string) =
    let segment = TextSegment(editor.CaretOffset, 1)
    // Create an invisible "find usages" marker. This is the only way to construct a TextSegmentMarker
    // without reflection hacks. VS checks that the marker is a TextSegmentMarker
    // https://github.com/mono/monodevelop/blob/61459958511d7ad4ea8debf4a59a77b1e98793fc/main/src/addins/MonoDevelop.SourceEditor2/MonoDevelop.SourceEditor/SourceEditorView.cs#L2744
    let marker = TextMarkerFactory.CreateUsageMarker(editor, Usage(segment, FindInFiles.ReferenceUsageType.Unknown))
    do
        marker.IsVisible <- false
        editor.AddMarker marker
    member x.Name = name
    member x.FileName = string editor.FileName.FullPath
    member x.Offset = marker.Offset
    member x.TextSegmentMarker = marker
    member x.Remove() =
        try
            editor.RemoveMarker marker |> ignore
        with
        | ex -> LoggingService.LogError (string ex)

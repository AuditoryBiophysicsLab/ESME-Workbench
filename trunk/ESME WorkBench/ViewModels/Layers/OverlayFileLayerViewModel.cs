using System.IO;
using ESME.Overlay;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Layers
{
    public class OverlayFileLayerViewModel : OverlayShapesLayerViewModel
    {
        public OverlayFileLayerViewModel(string overlayFileName) 
            : base(null, Path.GetFileNameWithoutExtension(overlayFileName))
        {
            Overlay = new LayerOverlay();
            Globals.MapViewModel.Overlays.Add(Overlay);

            var overlayFile = new OverlayFile(overlayFileName);
            foreach (var s in overlayFile.Shapes) OverlayShapes.Add(s);
            CommitShapes();
        }
    }
}
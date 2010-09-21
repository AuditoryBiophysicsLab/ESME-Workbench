using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Layers
{
    public class AdornmentLayerViewModel : LayerViewModel
    {
        public AdornmentLayerViewModel(string name, AdornmentLayer adornmentLayer) : base(name, null)
        {
            Overlay = Globals.MapViewModel.AdornmentOverlay;
            Globals.MapViewModel.AdornmentOverlay.Layers.Add(adornmentLayer);
        }
    }
}
using ESMEWorkBench.ViewModels.Main;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Layers
{
    public class AdornmentLayerViewModel : LayerViewModel
    {
        public AdornmentLayerViewModel(string name, AdornmentLayer adornmentLayer, MapViewModel mapViewModel) : base(name, null, mapViewModel)
        {
            Overlay = mapViewModel.AdornmentOverlay;
            mapViewModel.AdornmentOverlay.Layers.Add(adornmentLayer);
        }
    }
}
using System.IO;
using ESME.NEMO;
using ESME.Overlay;
using ESME.Platform;
using ESMEWorkBench.ViewModels.Main;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Layers
{
    public class ScenarioFileLayerViewModel : LayerViewModel
    {
        public ScenarioFileLayerViewModel(string nemoFileName, string nemoScenarioDirectory, MapViewModel mapViewModel) : base(Path.GetFileNameWithoutExtension(nemoFileName), nemoFileName, mapViewModel)
        {
            Overlay = new LayerOverlay
                      {
                          TileType = TileType.SingleTile
                      };
            mapViewModel.Overlays.Add(Overlay);

            var nemoFile = new NemoFile(nemoFileName, nemoScenarioDirectory);

            Children = new LayersCollection();
            var overlayLayer = new OverlayShapesLayerViewModel(Overlay, Path.GetFileNameWithoutExtension(nemoFile.Scenario.OverlayFile.FileName), mapViewModel);
            foreach (OverlayShape shape in nemoFile.Scenario.OverlayFile.Shapes) overlayLayer.OverlayShapes.Add(shape);
            overlayLayer.CommitShapes();
            Children.Add(overlayLayer);

            int platformCount = 0;
            foreach (NemoPlatform platform in nemoFile.Scenario.Platforms)
            {
                var behavior = new BehaviorModel(platform);
                var platformLayer = new OverlayShapesLayerViewModel(Overlay, "Platform " + platformCount + ": " + platform.Name + " course", mapViewModel);
                platformLayer.OverlayShapes.Add(behavior.CourseOverlay);
                platformLayer.OverlayShapes.Add(behavior.CourseStart);
                platformLayer.OverlayShapes.Add(behavior.CourseEnd);
                platformLayer.CommitShapes();
                Children.Add(platformLayer);
                foreach (NemoTrackdef trackdef in platform.Trackdefs)
                {
                    var opAreaLayer = new OverlayShapesLayerViewModel(Overlay, "Platform " + platformCount + ": " + platform.Name + " operational area", mapViewModel);
                    foreach (OverlayShape shape in trackdef.OverlayFile.Shapes) opAreaLayer.OverlayShapes.Add(shape);
                    opAreaLayer.CommitShapes();
                    Children.Add(opAreaLayer);
                }
                platformCount++;
            }
        }
    }
}
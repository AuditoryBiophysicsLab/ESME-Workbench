using System.IO;
using System.Windows.Media;
using ESME.NEMO;
using ESME.Platform;
using ESMEWorkBench.ViewModels.Layers;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Map
{
    public class ScenarioFileMapLayer : OverlayShapesMapLayer
    {
        public ScenarioFileMapLayer(string scenarioFileName, GeoCollection<Overlay> overlays, LayerViewModel layerViewModel)
        {
            FileName = scenarioFileName;
            Name = Path.GetFileNameWithoutExtension(scenarioFileName);
            layerViewModel.LayerName = Name;
            layerViewModel.Overlay = this;
            var nemoFile = new NemoFile(scenarioFileName, Globals.AppSettings.ScenarioDataDirectory);
            foreach (var shape in nemoFile.Scenario.OverlayFile.Shapes) OverlayShapes.Add(shape);
            CommitShapes();
            var platformCount = 0;
            foreach (var platform in nemoFile.Scenario.Platforms)
            {
                platformCount++;
                var behavior = new BehaviorModel(platform);
                var platformLayerName = "Platform " + platformCount + ": " + platform.Name;
                var platformLayerViewModel = new LayerViewModel(platformLayerName, null);
                layerViewModel.Children.Add(platformLayerViewModel);
                var platformLayer = new OverlayShapesMapLayer
                                    {
                                        LineStyle = new CustomStartEndLineStyle(PointSymbolType.Circle, Colors.Green, 5, PointSymbolType.Square, Colors.Red, 5, Colors.DarkGray, 1)
                                    };
                platformLayer.OverlayShapes.Add(behavior.CourseOverlay);
                platformLayer.CommitShapes();
                overlays.Add(platformLayerName, platformLayer);
                var courseLayerViewModel = new LayerViewModel("Track", platformLayer);
                platformLayerViewModel.Children.Add(courseLayerViewModel);
                var opAreaCount = 0;
                foreach (var trackdef in platform.Trackdefs)
                {
                    opAreaCount++;
                    var opAreaLayer = new OverlayShapesMapLayer();
                    foreach (var shape in trackdef.OverlayFile.Shapes) opAreaLayer.OverlayShapes.Add(shape);
                    opAreaLayer.CommitShapes();
                    var opAreaName = "Platform " + platformCount + ": " + platform.Name + " OpArea";
                    var opAreaLayerViewModel = platform.Trackdefs.Count == 1 ? new LayerViewModel("OpArea", opAreaLayer) : new LayerViewModel("OpArea " + opAreaCount, opAreaLayer);
                    platformLayerViewModel.Children.Add(opAreaLayerViewModel);
                    overlays.Add(opAreaName, opAreaLayer);
                }
            }
        }
    }
}
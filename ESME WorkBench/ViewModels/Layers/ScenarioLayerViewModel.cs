using System.IO;
using System.Windows.Media;
using ESME.NEMO;
using ESME.Platform;
using ESMEWorkBench.ViewModels.Map;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Layers
{
    public class ScenarioFileLayerViewModel : LayerViewModel
    {
        public ScenarioFileLayerViewModel(NemoFile nemoFile, GeoCollection<Overlay> overlays)
        {
            LayerName = Path.GetFileNameWithoutExtension(nemoFile.FileName);
            var overlay = new OverlayShapesMapLayer();
            Overlay = overlay;
            foreach (var shape in nemoFile.Scenario.OverlayFile.Shapes) overlay.OverlayShapes.Add(shape);
            overlay.CommitShapes();
            overlays.Add(overlay);
            var platformCount = 0;
            foreach (var platform in nemoFile.Scenario.Platforms)
            {
                platformCount++;
                var platformLayerName = "Platform " + platformCount + ": " + platform.Name;
                Children.Add(new ScenarioPlatformLayerViewModel(platform, platformLayerName, overlays));
            }
        }
    }

    public class ScenarioPlatformLayerViewModel : LayerViewModel
    {
        public ScenarioPlatformLayerViewModel(NemoPlatform nemoPlatform, string layerName, GeoCollection<Overlay> overlays) 
            : base(layerName)
        {
            var trackLayerViewModel = new PlatformTrackLayerViewModel(nemoPlatform, "Track", overlays);
            Children.Add(trackLayerViewModel);
            var opAreaCount = 0;
            foreach (var trackdef in nemoPlatform.Trackdefs)
            {
                opAreaCount++;
                var opAreaName = nemoPlatform.Trackdefs.Count == 1 ? "OpArea" : "OpArea " + opAreaCount;
                var opAreaLayerViewModel = new PlatformOpAreaLayerViewModel(trackdef, opAreaName, overlays);
                Children.Add(opAreaLayerViewModel);
            }
        }
    }

    public class PlatformOpAreaLayerViewModel : LayerViewModel
    {
        public PlatformOpAreaLayerViewModel(NemoTrackdef nemoTrackdef, string layerName, GeoCollection<Overlay> overlays)
            : base(layerName)
        {
            var overlay = new OverlayShapesMapLayer();
            foreach (var shape in nemoTrackdef.OverlayFile.Shapes) overlay.OverlayShapes.Add(shape);
            overlay.CommitShapes();
            overlays.Add(layerName + Path.GetFileNameWithoutExtension(Path.GetRandomFileName()), overlay);
        }
        
    }

    public class PlatformTrackLayerViewModel : LayerViewModel
    {
        public PlatformTrackLayerViewModel(NemoPlatform nemoPlatform, string layerName, GeoCollection<Overlay> overlays)
            : base(layerName)
        {
            var overlay = new OverlayShapesMapLayer
            {
                LineStyle = new CustomStartEndLineStyle(PointSymbolType.Circle, Colors.Green, 5, PointSymbolType.Square, Colors.Red, 5, Colors.DarkGray, 1)
            };
            var behavior = new BehaviorModel(nemoPlatform);
            overlay.OverlayShapes.Add(behavior.CourseOverlay);
            overlay.CommitShapes();
            overlays.Add(layerName + Path.GetFileNameWithoutExtension(Path.GetRandomFileName()), overlay);
        }
    }
}
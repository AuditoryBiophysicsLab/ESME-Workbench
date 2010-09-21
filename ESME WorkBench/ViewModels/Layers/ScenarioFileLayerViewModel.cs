using System.IO;
using System.Windows.Media;
using ESME.NEMO;
using ESME.Platform;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Layers
{
    public class ScenarioFileLayerViewModel : LayerViewModel
    {
        public ScenarioFileLayerViewModel(string nemoFileName, string nemoScenarioDirectory) 
            : base(Path.GetFileNameWithoutExtension(nemoFileName), nemoFileName)
        {
            Overlay = new LayerOverlay();
            Globals.MapViewModel.Overlays.Add(Overlay);

            var nemoFile = new NemoFile(nemoFileName, nemoScenarioDirectory);

            Children = new LayersCollection();
            var overlayLayer = new OverlayShapesLayerViewModel(Overlay, Path.GetFileNameWithoutExtension(nemoFile.Scenario.OverlayFile.FileName));
            foreach (var shape in nemoFile.Scenario.OverlayFile.Shapes) overlayLayer.OverlayShapes.Add(shape);
            overlayLayer.CommitShapes();
            Children.Add(overlayLayer);
            overlayLayer.Parent = this;

            var platformCount = 0;
            foreach (var platform in nemoFile.Scenario.Platforms)
            {
                var behavior = new BehaviorModel(platform);
                Overlay = new LayerOverlay();
                Globals.MapViewModel.Overlays.Add(Overlay);
                var platformLayer = new OverlayShapesLayerViewModel(Overlay, "Platform " + platformCount + ": " + platform.Name + " course")
                                    {
                                        LineStyle = new CustomStartEndLineStyle(PointSymbolType.Circle, Colors.Green, 5, PointSymbolType.Square, Colors.Red, 5, Colors.DarkGray, 1)
                                    };
                platformLayer.OverlayShapes.Add(behavior.CourseOverlay);
                //platformLayer.OverlayShapes.Add(behavior.CourseStart);
                //platformLayer.OverlayShapes.Add(behavior.CourseEnd);
                platformLayer.CommitShapes();
                Children.Add(platformLayer);
                platformLayer.Parent = this;
                foreach (var trackdef in platform.Trackdefs)
                {
                    Overlay = new LayerOverlay();
                    Globals.MapViewModel.Overlays.Add(Overlay);
                    var opAreaLayer = new OverlayShapesLayerViewModel(Overlay, "Platform " + platformCount + ": " + platform.Name + " operational area");
                    foreach (var shape in trackdef.OverlayFile.Shapes) opAreaLayer.OverlayShapes.Add(shape);
                    opAreaLayer.CommitShapes();
                    Children.Add(opAreaLayer);
                    opAreaLayer.Parent = this;
                }
                platformCount++;
            }
        }
    }
}
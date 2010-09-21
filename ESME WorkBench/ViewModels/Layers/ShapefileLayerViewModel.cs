using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;
using Cinch;
using ESMEWorkBench.ViewModels.Main;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Layers
{
    public class ShapefileLayerViewModel : LayerViewModel
    {
        public ShapefileLayerViewModel(string shapefileFileName) 
            : base(Path.GetFileNameWithoutExtension(shapefileFileName), shapefileFileName)
        {
            Overlay = new LayerOverlay();
            Globals.MapViewModel.Overlays.Add(Overlay);

            string projection = null;
            var projectionFile = Path.Combine(Path.GetDirectoryName(shapefileFileName), "projection.txt");
            if (File.Exists(projectionFile))
            {
                using (var sr = new StreamReader(projectionFile)) projection = sr.ReadToEnd();
            }
            var newLayer = new ShapeFileFeatureLayer(shapefileFileName);
            newLayer.ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = AreaStyles.County1;
            newLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            newLayer.RequireIndex = false;
            if (projection != null)
                newLayer.FeatureSource.Projection = new ManagedProj4Projection
                                                    {
                                                        InternalProjectionParameters = projection,
                                                        ExternalProjectionParameters = ManagedProj4Projection.GetEpsgParameters(4326),
                                                    };
            ((LayerOverlay) Overlay).Layers.Add(newLayer);
            ContextMenu.InsertRange(0, new List<MenuItemViewModel>
                                 {
                                     new MenuItemViewModel
                                     {
                                         Header = "Colors",
                                         Children = new List<MenuItemViewModel>
                                                    {
                                                        new MenuItemViewModel
                                                        {
                                                            Header = "Line color...",
                                                            Command = new SimpleCommand<object, object>(delegate
                                                                                                        {
                                                                                                            var cd = new ColorDialog
                                                                                                                     {
                                                                                                                         SolidColorOnly = true,
                                                                                                                         AnyColor = true,
                                                                                                                     };
                                                                                                            if (cd.ShowDialog() == DialogResult.OK) {}
                                                                                                        })
                                                        },
                                                        new MenuItemViewModel
                                                        {
                                                            Header = "Fill color...",
                                                            Command = new SimpleCommand<object, object>(delegate
                                                                                                        {
                                                                                                            var cd = new ColorDialog();
                                                                                                            if (cd.ShowDialog() == DialogResult.OK) {}
                                                                                                        })
                                                        }
                                                    }
                                     },
                                 });
            //WpfMap.Refresh();
        }
    }
}
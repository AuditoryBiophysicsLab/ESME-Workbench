using System;
using System.IO;
using ESME.Environment;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Layers
{
    public class RasterLayerViewModel : LayerViewModel
    {
        public RasterLayerViewModel(string layerName, string fileName)
            : base(layerName, fileName)
        {
            Bathymetry bathymetry;
            try
            {
                bathymetry = new Bathymetry(fileName);
            }
            catch (Exception e)
            {
                Globals.MessageBoxService.ShowError(string.Format("Error opening bathymetry file {0}:\n{1}",
                                                           fileName, e.Message));
                return;
            }
            var overlayLayer = new OverlayShapesLayerViewModel(null, Path.GetFileNameWithoutExtension(fileName) + " bathymetry bounds")
            {
                Overlay = new LayerOverlay(),
            };

            Globals.MapViewModel.Overlays.Add(overlayLayer.Overlay);
            overlayLayer.OverlayShapes.Add(bathymetry.BoundingBox);
            overlayLayer.CommitShapes();

            Globals.LayerDisplayViewModel.Layers.Add(overlayLayer);

            //Gets the Bounding box of the Polygonshape representing the extent of the image layer.
            var imageLayerRectangleShape = new RectangleShape(bathymetry.BoundingBox.West, bathymetry.BoundingBox.North, bathymetry.BoundingBox.East, bathymetry.BoundingBox.South);

            //Loads the ImageLayer with its extent as a the RectangleShape. 
            var gdiPlusRasterLayer = new GdiPlusRasterLayer(@"..\..\Data\World.tif", imageLayerRectangleShape)
                                     {
                                         UpperThreshold = double.MaxValue,
                                         LowerThreshold = 0
                                     };

            //The alternative way to load the image is with the acompanying world file. You do that by just specifying the image layer path and the accompanying world file
            //will be used. For more info, see http://en.wikipedia.org/wiki/World_file

            //GdiPlusRasterLayer gdiPlusRasterLayer = new GdiPlusRasterLayer(@"..\..\Data\World.tif");

            //The world file (.tfw) contains the world info as follow:
            //0.36000                   pixel size in the x-direction in map units/pixel
            //0                         rotation about y-axis (not used)
            //0                         rotation about x-axis (not used)
            //-0.36000                  pixel size in the y-direction in map units, almost always negative
            //-179.82000                x-coordinate of the center of the upper left pixel
            //89.8199999999999872       y-coordinate of the center of the upper left pixel

            var imageOverlay = new LayerOverlay();
            imageOverlay.Layers.Add("GdiPlusImageLayer", gdiPlusRasterLayer);

            //winformsMap1.Overlays.Add(imageOverlay);
            
        }
    }
}

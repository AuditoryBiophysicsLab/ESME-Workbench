using System.Collections;
using System.Collections.ObjectModel;
using System.IO;
using GisSharpBlog.NetTopologySuite.Geometries;
using GisSharpBlog.NetTopologySuite.Index.Strtree;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Map
{
    // IMPORTANT: You have to reference NetTopologySuite.dll & GeoApi.dll in your project.  They come with Map Suite.

    // This class provides a simple way of displaying a raster image on a map

    public class GeoRasterLayer : Layer
    {
        STRtree _spatialIndex;
        RectangleShape _boundingBox;
        GdiPlusRasterLayer _rasterLayer;

        public GeoRasterLayer() : this(string.Empty, double.MaxValue, double.MinValue) { }

        public GeoRasterLayer(string rasterFilename) : this(rasterFilename, double.MaxValue, double.MinValue) { }

        public GeoRasterLayer(string rasterFilename, double upperScale, double lowerScale)
        {
            RasterFilename = rasterFilename;
            UpperScale = upperScale;
            LowerScale = lowerScale;
            _boundingBox = new RectangleShape();
            PixelSize = 0;
        }

        #region public string RasterFilename { get; set; }

        public string RasterFilename
        {
            get { return _rasterFilename; }
            set
            {
                if (_rasterFilename == value) return;
                _rasterFilename = value;
                _rasterLayer = new GdiPlusRasterLayer(_rasterFilename)
                               {
                                   UpperThreshold = UpperScale,
                                   LowerThreshold = LowerScale,
                                   IsGrayscale = false
                               };
            }
        }

        string _rasterFilename;

        #endregion

        #region public Stream RasterStream { get; set; }

        public Stream RasterStream
        {
            get { return _rasterStream; }
            set
            {
                if (_rasterStream == value) return;
                _rasterStream = value;
                _rasterLayer = new GdiPlusRasterLayer("stream.bmp")
                               {
                                   UpperThreshold = UpperScale,
                                   LowerThreshold = LowerScale,
                                   IsGrayscale = false
                               };
                ((GdiPlusRasterSource)(_rasterLayer.ImageSource)).StreamLoading += (s, e) =>
                {
                    if (e.AlternateStreamName.Contains(".bmp"))
                        e.AlternateStream = _rasterStream;
                    else if (e.AlternateStreamName.Contains(".bpw"))
                    {
                        
                    }
                };
            }
        }

        Stream _rasterStream;

        #endregion

        public double UpperScale { get; set; }
        public double LowerScale { get; set; }

        public double North { get; set; }
        public double South { get; set; }
        public double East { get; set; }
        public double West { get; set; }
        
        /// <summary>
        /// Pixel size, in degrees.  This value is used to calculate the true bounding box extent in the case where
        /// North, South, East and West refer to pixel centers.  If this calculation should not be performed, this
        /// value should be set to zero
        /// </summary>
        public double PixelSize { get; set; }

        // Here in OpenCore we create the spatial index from North, South, East and West, which will be used the DrawCore later.
        protected override void OpenCore()
        {
            var halfPixel = PixelSize / 2.0;
            var north = North + halfPixel;
            var south = South - halfPixel;
            var east = East + halfPixel;
            var west = West - halfPixel;

            _spatialIndex.Insert(new Envelope(west, east, north, south), this);
            _spatialIndex.Build();
            _boundingBox = new RectangleShape(west, east, north, south);
        }

        // Here we set the spatial index to null to clean up the memory and get ready for serialization
        protected override void CloseCore() { _spatialIndex = null; }

        // When we get to the Draw, things are easy.  First we check to make sure we are within our scales.
        // Next we look up the Raster files in the spatial index,
        // then open their layer, call their Draw and close them.
        protected override void DrawCore(GeoCanvas canvas, Collection<SimpleCandidate> labelsInAllLayers)
        {
            var currentScale = ExtentHelper.GetScale(canvas.CurrentWorldExtent, canvas.Width, canvas.MapUnit);

            if (currentScale >= LowerScale && currentScale <= UpperScale)
            {
                var currentExtent = canvas.CurrentWorldExtent;
                var currentExtentEnvelope = new Envelope(currentExtent.UpperLeftPoint.X, currentExtent.LowerRightPoint.X, currentExtent.UpperLeftPoint.Y, currentExtent.LowerRightPoint.Y);
                var rasters = (ArrayList) _spatialIndex.Query(currentExtentEnvelope);
                //if (_spatialIndex.Query(currentExtentEnvelope).)

                foreach (string file in rasters)
                {
                    var rasterRasterLayer = new GdiPlusRasterLayer(file);
                    rasterRasterLayer.Open();
                    rasterRasterLayer.Draw(canvas, labelsInAllLayers);
                    rasterRasterLayer.Close();
                }
            }
        }

        // Here we let everyone know we support having a bounding box
        public override bool HasBoundingBox
        {
            get { return true; }
        }

        //  We use the cached bounding box we set in the OpenCore
        protected override RectangleShape GetBoundingBoxCore() { return _boundingBox; }
    }
}
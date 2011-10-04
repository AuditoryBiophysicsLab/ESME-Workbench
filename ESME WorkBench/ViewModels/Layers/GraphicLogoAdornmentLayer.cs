using System.Collections.ObjectModel;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using ThinkGeo.MapSuite.Core;

namespace OneNavyModel.ViewModels.Layers
{
    internal class GraphicLogoAdornmentLayer : AdornmentLayer
    {
        public GraphicLogoAdornmentLayer()
        {
            Location = AdornmentLocation.LowerRight;
        }

        public Bitmap Bitmap { get; set; }

        protected override void DrawCore(GeoCanvas canvas, Collection<SimpleCandidate> labelsInAllLayers)
        {
            if (!IsVisible) return;
            if (Bitmap == null) return;

            var screenPointF = GetDrawingLocation(canvas, Bitmap.Width, Bitmap.Height);

            // If the canvas happens to be using GDI+ then we can do an optimization and skip
            // the GeoImage.  Otherwise we go the longer route in the else statement
            if (canvas is GdiPlusGeoCanvas)
            {
                var gdiPlusGeoCanvas = canvas as GdiPlusGeoCanvas;
                gdiPlusGeoCanvas.DrawScreenImageWithoutScaling(Bitmap, screenPointF.X + Bitmap.Width * 0.5f,
                                                               screenPointF.Y + Bitmap.Height * 0.5f,
                                                               DrawingLevel.LevelOne, 0, 0, 0);
            }
            else
            {
                //  Here we have to convert the stream to a TIFF to be used in the GeoImage
                Stream stream = new MemoryStream();
                Bitmap.Save(stream, ImageFormat.Tiff);
                var geoImage = new GeoImage(stream);

                canvas.DrawScreenImageWithoutScaling(geoImage, screenPointF.X + Bitmap.Width * 0.5f,
                                                     screenPointF.Y + Bitmap.Height * 0.5f, DrawingLevel.LevelOne,
                                                     0, 0, 0);
            }
        }
    }
}
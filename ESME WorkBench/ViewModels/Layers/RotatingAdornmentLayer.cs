using System.Collections.ObjectModel;
using ThinkGeo.MapSuite.Core;

namespace OneNavyModel.ViewModels.Layers
{
    class RotatingAdornmentLayer : AdornmentLayer
    {
        GeoImage _imageToDraw;
        float _rotateAngle;

        public RotatingAdornmentLayer(GeoImage imageToDraw)
        {
            _imageToDraw = imageToDraw;
        }

        public GeoImage ImageToDraw
        {
            get { return _imageToDraw; }
            set { _imageToDraw = value; }
        }

        public float RotateAngle
        {
            get { return _rotateAngle; }
            set { _rotateAngle = value; }
        }

        protected override void DrawCore(GeoCanvas canvas, Collection<SimpleCandidate> labelsInAllLayers)
        {
            //Draws an unscaled image on the GeoCanvas with the rotateAngle according to RotateAngle property of RotatingAdornmentLayer
            //Here we draw at screen location 40, 40 but we could add a new Location property to give the user the flexibility
            //to adjust for the location of the image.
            canvas.DrawScreenImageWithoutScaling(_imageToDraw, 40, 40, DrawingLevel.LevelOne, 0, 0, _rotateAngle);
        }
    }
}

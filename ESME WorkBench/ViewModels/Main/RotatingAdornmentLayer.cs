using System.Collections.ObjectModel;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Main
{
    class RotatingAdornmentLayer : AdornmentLayer
    {
        GeoImage imageToDraw = null;
        float rotateAngle;

        public RotatingAdornmentLayer(GeoImage imageToDraw)
        {
            this.imageToDraw = imageToDraw;
        }

        public GeoImage ImageToDraw
        {
            get { return imageToDraw; }
            set { imageToDraw = value; }
        }

        public float RotateAngle
        {
            get { return rotateAngle; }
            set { rotateAngle = value; }
        }

        protected override void DrawCore(GeoCanvas canvas, Collection<SimpleCandidate> labelsInAllLayers)
        {
            //Draws an unscaled image on the GeoCanvas with the rotateAngle according to RotateAngle property of RotatingAdornmentLayer
            //Here we draw at screen location 40, 40 but we could add a new Location property to give the user the flexibility
            //to adjust for the location of the image.
            canvas.DrawScreenImageWithoutScaling(imageToDraw, 40, 40, DrawingLevel.LevelOne, 0, 0, rotateAngle);
        }
    }
}

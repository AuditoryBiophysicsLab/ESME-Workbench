namespace ESMEWorkBench.ViewModels.Main
{
#if false
    public class RadarRasterSource : GdiPlusRasterSource
    {
        RectangleShape boundingBox;
        BitmapSource sourceBitmap;

        string imagePathFileName;
        string worldFilePathFileName;

        [DllImport("gdi32.dll")]
        static extern bool DeleteObject(IntPtr hObject);
        
        public RadarRasterSource(string imagePathFileName, string worldFilePathFileName)
        {
            this.imagePathFileName = imagePathFileName;
            this.worldFilePathFileName = worldFilePathFileName;
        }

        protected override RectangleShape GetBoundingBoxCore()
        {
            if (boundingBox == null)
            {
                var worldFile = new WorldFile(File.ReadAllText(worldFilePathFileName));
                double upperLeftXMid = worldFile.UpperLeftX;
                double upperLeftYMid = worldFile.UpperLeftY;

                double horizontalResolution = worldFile.HorizontalResolution;
                double verticalResolution = Math.Abs(worldFile.VerticalResolution);

                var upperLeftX = upperLeftXMid - (horizontalResolution / 2);
                var upperLeftY = upperLeftYMid + (verticalResolution / 2);

                var lowerRightX = upperLeftX + (horizontalResolution * sourceBitmap.Width);
                var lowerRightY = upperLeftY - (verticalResolution * sourceBitmap.Height);

                boundingBox = new RectangleShape(upperLeftX, upperLeftY, lowerRightX, lowerRightY);
            }

            return boundingBox;
        }

        protected override GeoImage GetImageCore(RectangleShape worldExtent, int canvasWidth, int canvasHeight)
        {
            BitmapSource returnBitmap = null;
            var boundingBox = GetBoundingBox();

            try
            {
                RectangleShape intersection = Intersection(boundingBox, worldExtent);
                if (intersection.Width == 0 || intersection.Height == 0)
                {
                    returnBitmap = new WriteableBitmap(canvasWidth, canvasHeight, 96, 96, PixelFormats.Bgr32, null);
                }
                else
                {
                    returnBitmap = GetImageFromSpecifyArea(sourceBitmap, intersection, worldExtent, canvasWidth, canvasHeight);
                }

                Stream returnStream = GetStreamFromBitmap(returnBitmap);
                return new GeoImage(returnStream);
            }
        }

        public static RectangleShape Intersection(RectangleShape rectangle1, RectangleShape rectangle2)
        {
            int result = CheckContains(rectangle2, rectangle1);
            switch (result)
            {
                case 0://rectangleApartFrom:
                    return new RectangleShape();
                case 1://rectangleContaining:
                    return (RectangleShape)rectangle2.CloneDeep();
                case 2://rectangleContained:
                    return rectangle1;
                default:
                    // TODO : Move the code to rectangleSHape.
                    return GetIntersectionShape(rectangle2, rectangle1);
            }
        }

        private static RectangleShape GetIntersectionShape(RectangleShape targetShape, RectangleShape sourceShape)
        {
            double resultUpperLeftX = double.MaxValue;
            double resultUpperLeftY = double.MinValue;
            double resultLowerRightX = double.MinValue;
            double resultLowerRightY = double.MaxValue;

            double targetUpperLeftX = targetShape.UpperLeftPoint.X;
            double targetUpperLeftY = targetShape.UpperLeftPoint.Y;
            double targetLowerRightX = targetShape.LowerRightPoint.X;
            double targetLowerRightY = targetShape.LowerRightPoint.Y;

            double sourceUpperLeftX = sourceShape.UpperLeftPoint.X;
            double sourceUpperLeftY = sourceShape.UpperLeftPoint.Y;
            double sourceLowerRightX = sourceShape.LowerRightPoint.X;
            double sourceLowerRightY = sourceShape.LowerRightPoint.Y;

            resultUpperLeftX = (targetUpperLeftX > sourceUpperLeftX && targetUpperLeftX < sourceLowerRightX) ? targetUpperLeftX : sourceUpperLeftX;
            resultUpperLeftY = (targetUpperLeftY > sourceLowerRightY && targetUpperLeftY < sourceUpperLeftY) ? targetUpperLeftY : sourceUpperLeftY;
            resultLowerRightX = (targetLowerRightX > sourceUpperLeftX && targetLowerRightX < sourceLowerRightX) ? targetLowerRightX : sourceLowerRightX;
            resultLowerRightY = (targetLowerRightY > sourceLowerRightY && targetLowerRightY < sourceUpperLeftY) ? targetLowerRightY : sourceLowerRightY;

            RectangleShape result = new RectangleShape(resultUpperLeftX, resultUpperLeftY, resultLowerRightX, resultLowerRightY);
            return result;
        }

        private static int CheckContains(RectangleShape targetShape, RectangleShape sourceShape)
        {
            //rectangleApartFrom = 0;
            //rectangleContaining = 1; // Source Rectangle contains target rectangle.
            //rectangleContained = 2;  // Target Rectangle contains source rectangle.
            //rectangleIntersect = 3;

            int result = 3;

            bool validWidth = targetShape.Width <= sourceShape.Width;
            bool validHeight = targetShape.Height <= sourceShape.Height;
            double targetCenterX = (targetShape.UpperLeftPoint.X + targetShape.LowerRightPoint.X) / 2;
            double targetCenterY = (targetShape.UpperLeftPoint.Y + targetShape.LowerRightPoint.Y) / 2;
            double sourceCenterX = (sourceShape.UpperLeftPoint.X + sourceShape.LowerRightPoint.X) / 2;
            double sourceCenterY = (sourceShape.UpperLeftPoint.Y + sourceShape.LowerRightPoint.Y) / 2;
            double distanceX = Math.Abs(targetCenterX - sourceCenterX);
            double distanceY = Math.Abs(targetCenterY - sourceCenterY);

            if (validWidth && validHeight)
            {
                double minDistanceWidth = Math.Abs((targetShape.Width - sourceShape.Width) / 2);
                double minDistanceHeight = Math.Abs((targetShape.Height - sourceShape.Height) / 2);
                if (distanceX <= minDistanceWidth && distanceY <= minDistanceHeight)
                {
                    result = 1;
                }
            }
            else if (!validWidth && !validHeight)
            {
                double minDistanceWidth = Math.Abs((targetShape.Width - sourceShape.Width) / 2);
                double minDistanceHeight = Math.Abs((targetShape.Height - sourceShape.Height) / 2);
                if (distanceX <= minDistanceWidth && distanceY <= minDistanceHeight)
                {
                    result = 2;
                }
            }
            else
            {
                double maxDistanceWidth = Math.Abs((targetShape.Width + sourceShape.Width) / 2);
                double maxDistanceHeight = Math.Abs((targetShape.Height + sourceShape.Height) / 2);
                if (distanceX >= maxDistanceWidth || distanceY >= maxDistanceHeight)
                {
                    return 0;
                }
            }

            return result;
        }

        private static Stream GetStreamFromBitmap(Bitmap bitmap)
        {
            Stream returnStream = new MemoryStream();
            bitmap.Save(returnStream, ImageFormat.Png);
            returnStream.Seek(0, SeekOrigin.Begin);

            return returnStream;
        }

        private Bitmap GetImageFromSpecifyArea(Bitmap sourceBitmap, RectangleShape sourceRectangleInWorld, RectangleShape worldExtent, int canvasWidth, int canvasHeight)
        {
            double upperLeftX = sourceRectangleInWorld.UpperLeftPoint.X;
            double upperLeftY = sourceRectangleInWorld.UpperLeftPoint.Y;
            double sourceRectangleInWorldWidth = sourceRectangleInWorld.Width;
            double sourceRectangleInWorldHeight = sourceRectangleInWorld.Height;

            int bmpWidth = sourceBitmap.Width;
            int bmpHeight = sourceBitmap.Height;

            RectangleShape boundingBox = GetBoundingBox();

            int upperLeftXOnBitmap = (int)((upperLeftX - boundingBox.UpperLeftPoint.X) * bmpWidth / boundingBox.Width);
            int upperLeftYOnBitmap = (int)((boundingBox.UpperLeftPoint.Y - upperLeftY) * bmpHeight / boundingBox.Height);

            int areaWidth = (int)(sourceRectangleInWorldWidth * bmpWidth / boundingBox.Width);
            int areaHeight = (int)(sourceRectangleInWorldHeight * bmpHeight / boundingBox.Height);

            int tempReturnWidth = (int)Math.Round((sourceRectangleInWorldWidth * canvasWidth / worldExtent.Width));
            int tempReturnHeight = (int)Math.Round((sourceRectangleInWorldHeight * canvasHeight / worldExtent.Height));

            Bitmap returnImage;
            if (tempReturnWidth == 0 || tempReturnHeight == 0)
            {
                returnImage = new Bitmap(1, 1);
            }
            else
            {
                if (tempReturnWidth > canvasWidth) { tempReturnWidth = canvasWidth; }
                if (tempReturnHeight > canvasHeight) { tempReturnHeight = canvasHeight; }

                returnImage = new Bitmap(tempReturnWidth, tempReturnHeight);

                Point[] targetImagePoints = new Point[3];
                targetImagePoints[0] = new Point(0, 0);
                targetImagePoints[1] = new Point(tempReturnWidth, 0);
                targetImagePoints[2] = new Point(0, tempReturnHeight);

                Rectangle sourceRectangle = new Rectangle(upperLeftXOnBitmap, upperLeftYOnBitmap, areaWidth, areaHeight);

                Graphics graphics = null;

                try
                {
                    graphics = Graphics.FromImage(returnImage);
                    graphics.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.AntiAlias;
                    graphics.InterpolationMode = System.Drawing.Drawing2D.InterpolationMode.Bicubic;
                    graphics.InterpolationMode = System.Drawing.Drawing2D.InterpolationMode.NearestNeighbor;
                    graphics.DrawImage(sourceBitmap, targetImagePoints, sourceRectangle, GraphicsUnit.Pixel);
                }
                finally
                {
                    if (graphics != null) { graphics.Dispose(); }
                }
            }
            BitmapSource bitmapSource = System.Windows.Interop.Imaging.CreateBitmapSourceFromHBitmap(returnImage.GetHbitmap(), IntPtr.Zero, System.Windows.Int32Rect.Empty, BitmapSizeOptions.FromEmptyOptions());
            return returnImage;
        }

        protected override void OpenCore()
        {
            sourceBitmap = new Bitmap(imagePathFileName);
        }
    }


    public class RadarLayer : GdiPlusRasterLayer
    {
        public RadarLayer(string imagePathFileName, string worldFilePathFileName)
        {
            ImageSource = new RadarRasterSource(imagePathFileName, worldFilePathFileName);
        }

        protected override void DrawCore(GeoCanvas canvas, Collection<SimpleCandidate> labelsInAllLayers)
        {
            GeoImage geoImage = ImageSource.GetImage(canvas.CurrentWorldExtent, (int)canvas.Width, (int)canvas.Height);

            RectangleShape overlapShape = RadarRasterSource.Intersection(GetBoundingBox(), canvas.CurrentWorldExtent);
            PointShape centerPoint = overlapShape.GetCenterPoint();

            canvas.DrawWorldImageWithoutScaling(geoImage, centerPoint.X, centerPoint.Y, DrawingLevel.LevelOne);
        }
    }
#endif
}

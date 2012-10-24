using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

namespace EnvironmentBuilder.ClassControls
{
    class ImageView
    {
        #region Images View Private Data Members
        private Bitmap originalBitmap, intermediateBitmap;
        private RelativeSize relCoverage;
        private RelativePoint relLocation;
        private Size mViewSize;

        private int updating = 0;
        private float OriginalAspectRatio, ViewAspectRatio;
        private float zoomFactor = 1;
        
        private bool mPreserveAspectRatio = true;
        private bool maxIntermediateBitmap = false;

        private static RelativeSize FullCoverage = new RelativeSize(1, 1);
        private static RelativePoint UpperLeft = new RelativePoint(0, 0);
        private static RelativePoint LowerRight = new RelativePoint(1, 1);
        #endregion

        #region Images View Constructor
        /// <summary>
        /// Set Up the Bitmap Object and Initialize local members to default values. The Default will make the View include the entire image.
        /// </summary>
        /// <param name="OriginalBitmap">Original Bitmap to be viewed.</param>
        /// <param name="ViewSize">The Size of the control that will view the entire Original Map.</param>
        public ImageView(Bitmap OriginalBitmap, Size ViewSize)
        {
            originalBitmap = OriginalBitmap;
            relCoverage = FullCoverage;
            relLocation = UpperLeft;
            
            //Aspect Ratios
            OriginalAspectRatio = originalBitmap.Width / originalBitmap.Height;
            ViewAspectRatio = ViewSize.Width / ViewSize.Height;

            this.ViewSize = ViewSize;
        }

        #endregion

        #region Event Handlers
        public event EventHandler<EventArgs> ViewChanged;
        protected virtual void OnViewChanged()
        {

            RectangleF NewImage;
            RectangleF destRect;
            GraphicsUnit gUnit = GraphicsUnit.Pixel;

            ViewBitmap = new Bitmap(ViewSize.Width, ViewSize.Height);
            //if intermediatebm is zoomed all the way in, then don't

            Graphics g = Graphics.FromImage(ViewBitmap);
            g.Clear(Color.Black);

            NewImage = new RectangleF(relLocation * IntermediateSize, relCoverage * IntermediateSize);
            destRect = new RectangleF((ViewBitmap.Width - NewImage.Width) / 2f,
                (ViewBitmap.Height - NewImage.Height) / 2f, NewImage.Width, NewImage.Height);
            
            if ((IntermediateSize.Height < ViewSize.Height)) //Black Horizontal Bars
            {
                Blackorientation = BlackOrientation.Horizontal;
                relLocation = new RelativePoint(relLocation.X, 0);
                relCoverage = new RelativeSize(relCoverage.Width, 1);

                FirstBlack = new SizeF(IntermediateSize.Width, (ViewSize.Height - IntermediateSize.Height) / 2);
                SecondBlack = new SizeF(IntermediateSize.Width, (ViewSize.Height - IntermediateSize.Height) / 2);

                g.DrawImage(intermediateBitmap, destRect, NewImage, gUnit);
            }
            else if ((IntermediateSize.Width < ViewSize.Width)) //Black Veritcal Bars
            {
                Blackorientation = BlackOrientation.Vertical;
                relLocation = new RelativePoint(0, relLocation.Y);
                relCoverage = new RelativeSize(1, relCoverage.Height);

                FirstBlack = new SizeF((ViewSize.Width - IntermediateSize.Width) / 2, IntermediateSize.Height);
                SecondBlack = new SizeF((ViewSize.Width - IntermediateSize.Width) / 2, IntermediateSize.Height);

                g.DrawImage(intermediateBitmap, destRect, NewImage, gUnit);
            }
            else //No Bars
            {
                Blackorientation = null;
                g.DrawImage(intermediateBitmap, ViewBitmap.GetBounds(ref gUnit), NewImage, GraphicsUnit.Pixel);
            }

            if (ViewChanged != null)
                ViewChanged(this, new EventArgs());

        }
        #endregion

        #region Images View Public Properties


        public SizeF FirstBlack { get; private set; }

        public SizeF SecondBlack { get; private set; }
        public BlackOrientation? Blackorientation { get; private set; }

        /// <summary>
        /// Get a Re-Cloned View Bitmap from the Intermediate Bitmap.
        /// </summary>
        public Bitmap ViewBitmap { get; private set; }

        /// <summary>
        /// Get the Size in pixels of the Intermediate Bitmap.
        /// </summary>
        public Size IntermediateSize { get; private set; }

        /// <summary>
        /// Get or Set the View Size in pixels of the View Bitmap.
        /// </summary>
        public Size ViewSize
        {
            get { return mViewSize; }
            set 
            {
                Size LastViewSize;

                BeginUpdate();
                // TODO: Adjust the coverage value based on the new view size, use relative sizes
                LastViewSize = mViewSize;
                mViewSize = value;
                // If the view size has increased in either dimension, DeltaViewSize will show a positive value
                SizeF DeltaViewSize = new SizeF(mViewSize.Width - LastViewSize.Width, mViewSize.Height - LastViewSize.Height);
                ViewAspectRatio = mViewSize.Width / mViewSize.Height;
                if (relCoverage == FullCoverage)
                {
                    CreateIntermediateBitmap();
                }
                else
                {
                    RelativeSize DeltaCoverage = new RelativeSize(DeltaViewSize.Width / LastViewSize.Width, DeltaViewSize.Height / LastViewSize.Height) * relCoverage;
                    ViewCoverage += DeltaCoverage;
                }
                EndUpdate();
            }
        }
        /// <summary>
        /// Get or Set the Ratio of the Top Left Corner of the View Bitmap to the Top Left Corner of the Intermediate Bitmap.
        /// </summary>
        public RelativePoint ViewLocation
        {
            get { return relLocation; }
            set 
            {
                RelativePoint oldLocation = relLocation;
                relLocation = value;
                relLocation = RelativePoint.Max(relLocation, UpperLeft);
                relLocation = RelativePoint.Min(LowerRight - relCoverage, relLocation);
                if (relLocation != oldLocation)
                {
                    BeginUpdate();
                    EndUpdate();
                }
            }
        }

        //If Current View   
        public bool CanHScroll { get { return (relCoverage.Width < 1); } }
        public bool CanVScroll { get { return (relCoverage.Height < 1); } }

        /// <summary>
        /// Get the Horizontal and vertical coverage ratio. "View Width / Intermediate Width" and "View Height / Intermediate Height".
        /// </summary>
        public RelativeSize ViewCoverage
        {
            get { return relCoverage; }
            set
            {
                RelativeSize oldCoverage = relCoverage;
                relCoverage = RelativeSize.Min(value, FullCoverage);
                if (oldCoverage != relCoverage)
                {
                    BeginUpdate();
                    EndUpdate();
                }
            }
        }

        /// <summary>
        /// Get or Set the Zoom Factor used to Zoom in into or out of the Bitmap.
        /// </summary>
        public float ZoomFactor
        {
            get { return zoomFactor; }
            set
            {
                BeginUpdate();
                float oldZoomFactor = zoomFactor;
                RelativeSize oldCoverage = ViewCoverage;
                bool intermediateBitmapIsMaxSize = false;

               /* if (intermediateBitmap.Size == originalBitmap.Size)
                    intermediateBitmapIsMaxSize = true;*/
                
                // if we're not max size, we can zoom in, OR 
                // if we ARE max size and we're zooming OUT (towards a zoom factor of 1)
                if (!intermediateBitmapIsMaxSize || (value < oldZoomFactor))
                {
                    // Set the new value, keeping in mind the MINIMUM allowable zoom factor is 1 (full view)
                    zoomFactor = Math.Max(value, 1);

                    // Create a new intermediate bitmap
                    CreateIntermediateBitmap();

                    // Calculate the new coverage
                    ViewCoverage = FullCoverage / zoomFactor;

                    // ***SUSPECT AREA***
                    RelativeSize deltaLoc = (oldCoverage - ViewCoverage) / 2f;
                    ViewLocation += deltaLoc;
                }
                EndUpdate();
            }
        }

        /// <summary>
        /// Get or Set Preserving the Aspect Ratio of the Bitmap.
        /// </summary>
        public bool PreserveAspectRatio             // ***NOT USED YET***, was thought of as a user option.
        {
            get { return mPreserveAspectRatio; }
            set { mPreserveAspectRatio = value; }
        }

        #endregion

        #region Images View Public Methods

        public void ZoomTo(PointF TopLeftRatio, float ZoomFactor)
        {
            RelativeSize oldCoverage = relCoverage;
            if (intermediateBitmap.Size == originalBitmap.Size)
                //    intermediateBitmapIsMaxSize = true;
                return;
            
            this.zoomFactor = Math.Max(ZoomFactor, 1);
            CreateIntermediateBitmap();
            ViewCoverage = FullCoverage / zoomFactor;
            this.ViewLocation += (RelativePoint)TopLeftRatio * oldCoverage;
        }

        #endregion

        #region Images View Private Methods and Properties

        private void BeginUpdate()
        {
            updating++;
        }

        private void EndUpdate()
        {
            updating--;
            if (updating <= 0)
            {
                OnViewChanged();
                updating = 0;
            }
        }

        private bool MaxIntermediateBitmap { get { return maxIntermediateBitmap; } }

        private void CreateIntermediateBitmap()
        {
            Bitmap oldIntermediateBitmap; 
            Size intSize = IntermediateSize;
            oldIntermediateBitmap = intermediateBitmap;

            if (mPreserveAspectRatio)
            {
                if (OriginalAspectRatio > ViewAspectRatio)
                {
                    // If the original is proportionally wider than the current view
                    intSize.Width = (int)(ViewSize.Width * ZoomFactor);
                    intSize.Height = (int)(intSize.Width / OriginalAspectRatio);
                }
                else
                {
                    // If the original is proportionally taller than the current view
                    intSize.Height = (int)(ViewSize.Height * ZoomFactor);
                    intSize.Width = (int)(intSize.Height * OriginalAspectRatio);
                }
            }
            else
            {
                intSize.Width = (int)(ViewSize.Width * ZoomFactor);
                intSize.Height = (int)(ViewSize.Height * ZoomFactor);
            }

            intSize.Width = Math.Min(intSize.Width, originalBitmap.Width);
            intSize.Height = Math.Min(intSize.Height, originalBitmap.Height);
            IntermediateSize = intSize;
            
            if(intermediateBitmap == null || (IntermediateSize.Width != intermediateBitmap.Width))            
                intermediateBitmap = new Bitmap(originalBitmap, IntermediateSize.Width, IntermediateSize.Height);
            if (intermediateBitmap.Size == originalBitmap.Size) 
                maxIntermediateBitmap = true;
        }

        

        #endregion
    }

    internal enum BlackOrientation
    {
        Horizontal,
        Vertical
    }
}


using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using HRC.Navigation;
using Color = System.Windows.Media.Color;

namespace ESME.Overlay
{
    public abstract class OverlayShape
    {
        protected List<EarthCoordinate> EarthCoordinates;
        protected string MyWellKnownText;

        protected OverlayShape()
            : this(Colors.Black, 1f)
        {
        }

        protected OverlayShape(Color color)
            : this(color, 1f)
        {
        }

        protected OverlayShape(Color color, float width)
            : this(color, width, LineStyle.Solid)
        {
        }

        protected OverlayShape(Color color, float width, LineStyle lineStyle)
        {
            Color = color;
            Width = width;
            LineStyle = lineStyle;
            EarthCoordinates = new List<EarthCoordinate>();
        }
        public virtual bool IsClosed
        {
            get { throw new NotImplementedException(); }
            protected set {throw new NotImplementedException(); }
        }
        public virtual bool HasCrossingSegments
        {
            get { throw new NotImplementedException(); }
            protected set { throw new NotImplementedException(); }
        }
        public virtual bool IsUsableAsPerimeter
        {
            get { throw new NotImplementedException(); }
            protected set {throw new NotImplementedException(); }
        }
        public virtual bool Contains(EarthCoordinate location) { throw new NotImplementedException(); }
        public virtual EarthCoordinate Bounce(EarthCoordinate startLocation, EarthCoordinate proposedEndLocation) { throw new NotImplementedException(); }
        public abstract string WellKnownText { get; }
        protected Rect BoundingBox { get; private set; }
        public Color Color { get; set; }

        public float Width { get; set; }

        public LineStyle LineStyle { get; set; }

        public EarthCoordinate Location
        {
            get { return EarthCoordinates[0]; }
            //set { Move(value); }
        }

        // I made all the Move methods private because I think the overlay classes should end up being
        // immutable.  The derived classes are doing things like storing shapes and colors for rendering.
        // It's possible to re-expose this in the future, but more work needs to be done to make sure the derived classes
        // will do the right thing when moved.

        protected EarthCoordinate this[int index]
        {
            get { return EarthCoordinates[index]; }
            set { EarthCoordinates[index] = value; }
        }

        protected int Length
        {
            get { return EarthCoordinates.Count(); }
        }

        private void Move(EarthCoordinate newLocation)
        {
            double bearingDegrees = EarthCoordinates[0].GetBearingTo_Degrees(newLocation);
            double distanceMeters = EarthCoordinates[0].GetDistanceTo_Meters(newLocation);

            Move(bearingDegrees, distanceMeters);
        }

        private void Move(double bearingDegrees, double distanceMeters)
        {
            foreach (var cur in EarthCoordinates)
                cur.Move(bearingDegrees, distanceMeters);
        }

        protected void Add(EarthCoordinate newPoint)
        {
            EarthCoordinates.Add(newPoint);
            if (EarthCoordinates.Count > 1)
                CalculateBoundingBox();
        }

        protected void Add(EarthCoordinate[] newPoints)
        {
            EarthCoordinates.AddRange(newPoints);
            CalculateBoundingBox();
        }

        private void CalculateBoundingBox()
        {
            var north = -90.0;
            var south = 90.0;
            var east = -180.0;
            var west = 180.0;
            foreach (var curPoint in EarthCoordinates)
            {
                north = Math.Max(curPoint.Latitude_degrees, north);
                south = Math.Min(curPoint.Latitude_degrees, south);
                east = Math.Max(curPoint.Longitude_degrees, east);
                west = Math.Min(curPoint.Longitude_degrees, west);
            }
            var width = Math.Abs(west - east);
            var height = Math.Abs(north - south);
            BoundingBox = new Rect(west, north, width, height);
        }
    }
}
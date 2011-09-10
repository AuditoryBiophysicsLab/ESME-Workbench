using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using HRC.Navigation;

namespace ESME.NEMO.Overlay
{
    public abstract class OverlayShape
    {
        protected List<EarthCoordinate> _earthCoordinates;
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
            _earthCoordinates = new List<EarthCoordinate>();
        }

        public virtual List<EarthCoordinate> EarthCoordinates { get { return new List<EarthCoordinate>(_earthCoordinates); } }
        public virtual List<Geo> Geos { get { return new List<Geo>(_earthCoordinates); } }

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
        public Rect BoundingBox { get; private set; }
        public Color Color { get; set; }

        public float Width { get; set; }

        public LineStyle LineStyle { get; set; }

        public EarthCoordinate Location
        {
            get { return _earthCoordinates[0]; }
            //set { Move(value); }
        }

        // I made all the Move methods private because I think the overlay classes should end up being
        // immutable.  The derived classes are doing things like storing shapes and colors for rendering.
        // It's possible to re-expose this in the future, but more work needs to be done to make sure the derived classes
        // will do the right thing when moved.

        protected EarthCoordinate this[int index]
        {
            get { return _earthCoordinates[index]; }
            set { _earthCoordinates[index] = value; }
        }

        protected int Length
        {
            get { return _earthCoordinates.Count(); }
        }

        private void Move(EarthCoordinate newLocation)
        {
            var bearingDegrees = _earthCoordinates[0].BearingTo(newLocation);
            var distanceMeters = _earthCoordinates[0].DistanceTo(newLocation);

            Move(bearingDegrees, distanceMeters);
        }

        private void Move(double bearingDegrees, double distanceMeters)
        {
            foreach (var cur in _earthCoordinates)
                cur.Move(bearingDegrees, distanceMeters);
        }

        protected void Add(EarthCoordinate newPoint)
        {
            _earthCoordinates.Add(newPoint);
            if (_earthCoordinates.Count > 1)
                CalculateBoundingBox();
        }

        protected void Add(EarthCoordinate[] newPoints)
        {
            _earthCoordinates.AddRange(newPoints);
            CalculateBoundingBox();
        }

        protected void Add(IEnumerable<EarthCoordinate> newPoints)
        {
            _earthCoordinates.AddRange(newPoints);
            CalculateBoundingBox();
        }

        protected void Add(IEnumerable<Geo> newPoints)
        {
            _earthCoordinates.AddRange(newPoints.Cast<EarthCoordinate>());
            CalculateBoundingBox();
        }

        private void CalculateBoundingBox()
        {
            var north = -90.0;
            var south = 90.0;
            var east = -180.0;
            var west = 180.0;
            foreach (var curPoint in _earthCoordinates)
            {
                north = Math.Max(curPoint.Latitude, north);
                south = Math.Min(curPoint.Latitude, south);
                east = Math.Max(curPoint.Longitude, east);
                west = Math.Min(curPoint.Longitude, west);
            }
            var width = Math.Abs(west - east);
            var height = Math.Abs(north - south);
            BoundingBox = new Rect(west, south, width, height);
        }
    }
}
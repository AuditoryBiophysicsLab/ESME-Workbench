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
        protected List<Geo> _geos;

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
            _geos = new List<Geo>();
        }

        public virtual List<Geo> Geos { get { return new List<Geo>(_geos); } }

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
        public virtual bool Contains(Geo location) { throw new NotImplementedException(); }
        public virtual Geo Bounce(Geo startLocation, Geo proposedEndLocation) { throw new NotImplementedException(); }
        public abstract string WellKnownText { get; }
        public Rect BoundingBox { get; private set; }
        public Color Color { get; set; }

        public float Width { get; set; }

        public LineStyle LineStyle { get; set; }

        public Geo Location
        {
            get { return _geos[0]; }
            //set { Move(value); }
        }

        // I made all the Move methods private because I think the overlay classes should end up being
        // immutable.  The derived classes are doing things like storing shapes and colors for rendering.
        // It's possible to re-expose this in the future, but more work needs to be done to make sure the derived classes
        // will do the right thing when moved.

        protected Geo this[int index]
        {
            get { return _geos[index]; }
            set { _geos[index] = value; }
        }

        protected int Length
        {
            get { return _geos.Count(); }
        }
#if false
        private void Move(Geo newLocation)
        {
            _geos[0].Offset(_geos[0].DistanceRadians(newLocation), _geos[0].Azimuth(newLocation));
            var bearingDegrees = _geos[0].BearingTo(newLocation);
            var distanceMeters = _geos[0].DistanceTo(newLocation);

            Move(bearingDegrees, distanceMeters);
        }

        private void Move(double bearingDegrees, double distanceMeters)
        {
            foreach (var cur in _geos)
                cur.Move(bearingDegrees, distanceMeters);
        }
#endif

        protected void Add(Geo newPoint)
        {
            _geos.Add(newPoint);
            if (_geos.Count > 1)
                CalculateBoundingBox();
        }

        protected void Add(Geo[] newPoints)
        {
            _geos.AddRange(newPoints);
            CalculateBoundingBox();
        }

        protected void Add(IEnumerable<Geo> newPoints)
        {
            _geos.AddRange(newPoints);
            CalculateBoundingBox();
        }

        private void CalculateBoundingBox()
        {
            var north = -90.0;
            var south = 90.0;
            var east = -180.0;
            var west = 180.0;
            foreach (var curPoint in _geos)
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
using System;
using System.Windows.Media;
using System.Text;
using HRC.Navigation;

namespace ESME.NEMO.Overlay
{
    public class OverlayLineSegment : OverlayShape
    {
        public OverlayLineSegment(Geo p1, Geo p2)
            : this(p1, p2, Colors.Black, 1f, LineStyle.Solid)
        {
        }

        public OverlayLineSegment(Geo p1, Geo p2, Color color, float width, LineStyle lineStyle)
            : base(color, width, lineStyle)
        {
            Add(p1);
            Add(p2);

            Course = new Course(p1, p2);
            ComputeCoefficients();
        }

        public Course Course { get; private set; }

        public override string ToString()
        {
            return String.Format(" Start: ({0}, {1})\n    End: ({2}, {3})\nLength: {4}km\nCourse: {5}deg",
                                 this[0].Latitude, this[0].Longitude,
                                 this[1].Latitude, this[1].Longitude,
                                 this[0].DistanceKilometers(this[1]),
                                 Course.Degrees);
        }

        public override string WellKnownText
        {
            get
            {
                if (_geos.Count < 2) return null;
                if (MyWellKnownText == null)
                {
                    var retval = new StringBuilder();
                    retval.Append("LINESTRING(");
                    foreach (var coord in _geos)
                        retval.Append(string.Format("{0} {1}, ", coord.Latitude, coord.Longitude));
                    retval.Append(string.Format("{0} {1}, ", _geos[0].Latitude, _geos[0].Longitude));
                    retval.Remove(retval.Length - 2, 2); // Lose the last comma and space
                    retval.Append(")");
                    MyWellKnownText = retval.ToString();
                }
                return MyWellKnownText;
            }
        }


        #region Internal properties, methods and functions available for use by other classes in this assembly

        /// <summary>
        /// True if the segment is vertical (both X-coordinates are equal)
        /// </summary>
        internal bool IsVertical { get; private set; }

        /// <summary>
        /// True if the segment if horizontal (both Y-coordinates are equal)
        /// </summary>
        internal bool IsHorizontal { get; private set; }

        /// <summary>
        /// Returns the midpoint of the segment
        /// </summary>
        internal Geo Midpoint
        {
            get
            {
                return new Geo(
                    (this[0].Latitude + this[1].Latitude)/2.0,
                    (this[0].Longitude + this[1].Longitude)/2.0);
            }
        }

        /// <summary>
        /// True if the two segments intersect
        /// </summary>
        /// <param name="that">The other segment</param>
        /// <returns></returns>
        internal bool Intersects(OverlayLineSegment that)
        {
            //Console.WriteLine("Checking intersection of THIS segment " + this.ToString());
            //Console.WriteLine("with THAT segment " + that.ToString());
            // Quick check to see if the bounding box of either line segment intersects.
            //if (BoundingBox.IntersectsWith(that.BoundingBox))
            //{
            // If the bounding boxes DO intersect, the segments themselves MAY intersect

            // If the lines overlap, they do intersect
            if (Overlaps(that))
                return true;

            // If the lines do not overlap, and the bounding box of this point
            // contains the intersection point, then they DO intersect
            var intersectionPoint = IntersectionPoint(that);
            //Console.WriteLine("Intersection point calculated to be ({0}, {1})", IntersectionPoint.Latitude, IntersectionPoint.Longitude);
            if ((intersectionPoint != null) && (BoundingBox.Contains(intersectionPoint) && that.BoundingBox.Contains(intersectionPoint)))
                return true;
            //if (IntersectionPoint.Longitude < BoundingBox.Left)
            //    Console.WriteLine("Intersection point is LEFT of the bounding box by {0}", BoundingBox.Left - IntersectionPoint.Longitude);
            //if (IntersectionPoint.Longitude > BoundingBox.Right)
            //    Console.WriteLine("Intersection point is RIGHT of the bounding box by {0}", IntersectionPoint.Longitude - BoundingBox.Right);
            //if (IntersectionPoint.Latitude > BoundingBox.Top)
            //    Console.WriteLine("Intersection point is ABOVE the bounding box by {0}", IntersectionPoint.Latitude - BoundingBox.Top);
            //if (IntersectionPoint.Latitude < BoundingBox.Bottom)
            //    Console.WriteLine("Intersection point is BELOW the bounding box by {0}", BoundingBox.Bottom - IntersectionPoint.Latitude);
            //}
            // If none of the above conditions were previously met, the segments DO NOT intersect
            return false;
        }

        /// <summary>
        /// True if the two segments overlap
        /// </summary>
        /// <param name="that">The other segment</param>
        /// <returns></returns>
        internal bool Overlaps(OverlayLineSegment that)
        {
            // For two segments to overlap, they must first be colinear
            if (IsColinearWith(that))
            {
                // If they are colinear, check if our bounding box contains either of the other segment's endpoints
                if (BoundingBox.Contains(that._geos[0]) ||
                    (BoundingBox.Contains(that._geos[1])))
                    // If it does, we overlap the other segment
                    return true;

                // If it doesn't, then check if the other segment overlaps us.  This can happen if the other segment
                // completely contains us, and is also longer than us. If that turns out to be the case, then one or
                // both of our endpoints will be contained in the other segment's bounding box.
                if (that.BoundingBox.Contains(_geos[0]) ||
                    (that.BoundingBox.Contains(_geos[1])))
                    // If it does, we overlap the other segment
                    return true;
            }
            return false;
        }

        /// <summary>
        /// True if the two segments are on the same line (not necessarily overlapping)
        /// </summary>
        /// <param name="that">The other segment</param>
        /// <returns></returns>
        internal bool IsColinearWith(OverlayLineSegment that)
        {
            if (IsVertical && that.IsVertical)
            {
                if (Math.Abs(_geos[0].Longitude - that._geos[0].Longitude) < 0.0001)
                    return true;
            }
            if (IsParallelTo(that))
            {
                if (Math.Abs(_b - that._b) < 0.0001)
                    return true;
            }
            return false;
        }

        /// <summary>
        /// True if the two segments are parallel to each other
        /// </summary>
        /// <param name="that">The other segment</param>
        /// <returns></returns>
        internal bool IsParallelTo(OverlayLineSegment that)
        {
            if (IsVertical && that.IsVertical)
                return true;
            if (Math.Abs(_m - that._m) < 0.0001)
                return true;
            return false;
        }

        #endregion

        #region Private properties, utility methods and functions used by this class only

        private double _b;
        private double _m;

        private void ComputeCoefficients()
        {
            // Initialize these to false, they are only true if the tests below are true;
            IsVertical = IsHorizontal = false;

            // If both X values are NOT the same
            if (Math.Abs(this[0].Longitude - this[1].Longitude) > 0.0001)
            {
                // Classic linear equation:
                // y = m * x + b
                // solving for b yields:
                // y - (m * x) = b

                // We can compute the slope of the line
                _m = (this[1].Latitude - this[0].Latitude)/
                     (this[1].Longitude - this[0].Longitude);
                // and its' y-intercept
                _b = this[0].Latitude - (_m*this[0].Longitude);

                if (Math.Abs(_m - 0.0) < 0.0001)
                    IsHorizontal = true;
            }
            else
            {
                // if both X values ARE the same, the line is vertical, and both m and b are undefined
                _m = double.NaN;
                _b = double.NaN;
                IsVertical = true;
            }
        }

        /// <summary>
        /// Returns a Y value given an X value for Y = M * X + B
        /// </summary>
        /// <param name="x"></param>
        /// <returns></returns>
        private double Y(double x)
        {
            if (IsVertical)
                throw new AlgebraicException("Can't solve for Y on a vertical line");
            return (_m*x) + _b;
        }

        /// <summary>
        /// Returns an X value given a Y value for X = (Y - B) / M
        /// </summary>
        /// <param name="y"></param>
        /// <returns></returns>
        private double X(double y)
        {
            if (IsHorizontal)
                throw new AlgebraicException("Can't solve for X on a horizontal line");
            return (y - _b)/_m;
        }

        public Geo IntersectionPoint(OverlayLineSegment that)
        {
            if (IsParallelTo(that))
            {
                if (IsColinearWith(that))
                {
                    if (BoundingBox.Contains(that[0])) return that[0];
                    if (BoundingBox.Contains(that[1])) return that[1];
                    if (that.BoundingBox.Contains(this[0])) return this[0];
                    if (that.BoundingBox.Contains(this[1])) return this[1];
                }
                return null;
                //throw new GeometricException(
                //    "OverlayLineSegment: Lines are parallel but not colinear, they do not intersect");
            }
            // After we pass the above tests, we know the lines represented by our segments DO intersect somewhere
            // Now we will figure out exactly where.

            if (IsVertical)
                return new Geo(that.Y(this[0].Longitude), this[0].Longitude);
            if (that.IsVertical)
                return new Geo(Y(that[0].Longitude), that[0].Longitude);
            //if (this.IsHorizontal)
            //    return new PointF(that.X(this.Points[0].Y), this.Points[0].Y);

            // Algebra to solve for the x-coordinate intersection point
            // set the y-coordinates equal to each other:
            // this.m*x + this.b = that.m*x + that.b
            // subtract that.m*x from both sides, yielding:
            // (this.m - that.m)*x + this.b = that.b
            // subtract this.b from both sides, yielding:
            // (this.m - that.m)*x = that.b - this.b
            // divide both sides by this.m - that.m, yielding:
            // x = (that.b - this.b) / (this.m - that.m)
            var xIntersect = (that._b - _b)/(_m - that._m);

            // now that we have xIntersect, we solve for y by plugging in xIntersect into
            // either one of our line equations.  Arbitrarily, we'll pick the first one.
            var yIntersect = Y(xIntersect);

            return new Geo(yIntersect, xIntersect);

            //return new PointF((float)xIntersect, (float)yIntersect);
        }

        public override bool Contains(Geo pointToTest)
        {
            var verticalMatch = false;
            var horizontalMatch = false;
            // We don't know or care if this line segment is drawn west to east in longitude, so we will test for both possibilities.
            // The same thing goes for north and south with respect to latitude.

            // If one or the other is true, then the test is deemed to have succeeded.  If neither are true, the test fails.

            // If the point's longitude is within the span of longitudes covered by this line segment
            if (IsVertical)
            {
                var deltaLon = Math.Abs(this[0].Longitude - pointToTest.Longitude);
                if (deltaLon < 1e-6) verticalMatch = true;
            }
            else
            {
                if (((this[0].Longitude <= pointToTest.Longitude) && (pointToTest.Longitude <= this[1].Longitude)) ||
                    ((this[1].Longitude <= pointToTest.Longitude) && (pointToTest.Longitude <= this[0].Longitude))) 
                    verticalMatch = true;
            }
            if (IsHorizontal)
            {
                var deltaLat = Math.Abs(this[0].Latitude - pointToTest.Latitude);
                if (deltaLat < 1e-6) horizontalMatch = true;
            }
            else
            {
                // If the point's latitude is within the span of latitudes covered by this line segment
                if (((this[0].Latitude <= pointToTest.Latitude) && (pointToTest.Latitude <= this[1].Latitude)) || 
                    ((this[1].Latitude <= pointToTest.Latitude) && (pointToTest.Latitude <= this[0].Latitude))) 
                    horizontalMatch = true;
            }
            return verticalMatch && horizontalMatch; // The line segment does not contain the point
        }

        #endregion
    }
}
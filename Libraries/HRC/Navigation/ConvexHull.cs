using System;
using System.Collections.Generic;
using System.Linq;
using C5;

namespace HRC.Navigation
{
    // An implementation of Graham's (1972) point elimination algorithm,
    // as modified by Andrew (1979) to find lower and upper hull separately.

    // This implementation correctly handle duplicate points, and
    // multiple points with the same x-coordinate.

    // 1. Sort the points lexicographically by increasing (x,y), thus 
    //    finding also a leftmost point L and a rightmost point R.
    // 2. Partition the point set into two lists, upper and lower, according as 
    //    point is above or below the segment LR.  The upper list begins with 
    //    L and ends with R; the lower list begins with R and ends with L.
    // 3. Traverse the point lists clockwise, eliminating all but the extreme
    //    points (thus eliminating also duplicate points).
    // 4. Join the point lists (in clockwise order) in an array, 
    //    leaving out L from lower and R from upper.
    public static class ConvexHull
    {
        public static List<Geo> Create(List<Geo> sourcePoints, bool makeClosed = false)
        {
            var points = new ConvexHullPoint[sourcePoints.Count];
            for (var i = 0; i < points.Length; i++)
                points[i] = new ConvexHullPoint(sourcePoints[i].Longitude, sourcePoints[i].Latitude);
            var resultPoints = Create(points);
            var result = resultPoints.Select(t => new Geo(t.Y, t.X)).ToList();
            if (makeClosed) result.Add(result.First());
            return result;
        }

        static IEnumerable<ConvexHullPoint> Create(ConvexHullPoint[] pts)
        {
            // 1. Sort ConvexHullPoints lexicographically by increasing (x, y)
            var n = pts.Length;
            Array.Sort(pts);
            ConvexHullPoint left = pts[0], right = pts[n - 1];
            // 2. Partition into lower hull and upper hull
            C5.IList<ConvexHullPoint> lower = new C5.LinkedList<ConvexHullPoint>();
            C5.IList<ConvexHullPoint> upper = new C5.LinkedList<ConvexHullPoint>();
            lower.InsertFirst(left); upper.InsertLast(left);
            for (var i = 0; i < n; i++)
            {
                var det = ConvexHullPoint.Area2(left, right, pts[i]);
                if (det < 0)
                    lower.InsertFirst(pts[i]);
                else if (det > 0)
                    upper.InsertLast(pts[i]);
            }
            lower.InsertFirst(right);
            upper.InsertLast(right);
            // 3. Eliminate points not on the hull
            Eliminate(lower);
            Eliminate(upper);
            // 4. Join the lower and upper hull, leaving out lower.Last and upper.Last
            var res = new ConvexHullPoint[lower.Count + upper.Count - 2];
            lower[0, lower.Count - 1].CopyTo(res, 0);
            upper[0, upper.Count - 1].CopyTo(res, lower.Count - 1);
            return res;
        }

        // Graham's scan
        public static void Eliminate(C5.IList<ConvexHullPoint> lst)
        {
            var view = lst.View(0, 0);
            var slide = 0;
            while (view.TrySlide(slide, 3))
                if (ConvexHullPoint.Area2(view[0], view[1], view[2]) < 0)   // right turn
                    slide = 1;
                else
                {                                                 // left or straight
                    view.RemoveAt(1);
                    slide = view.Offset != 0 ? -1 : 0;
                }
        }
    }

    // ------------------------------------------------------------

    // Points in the plane

    public class ConvexHullPoint : IComparable<ConvexHullPoint>
    {
        private static readonly C5Random Rnd = new C5Random(42);

        public readonly double X, Y;

        public ConvexHullPoint(double x, double y)
        {
            X = x; Y = y;
        }

        public override string ToString()
        {
            return "(" + X + ", " + Y + ")";
        }

        public static ConvexHullPoint Random(int w, int h)
        {
            return new ConvexHullPoint(Rnd.Next(w), Rnd.Next(h));
        }

        public bool Equals(ConvexHullPoint p2)
        {
            return X == p2.X && Y == p2.Y;
        }

        public int CompareTo(ConvexHullPoint p2)
        {
            var major = X.CompareTo(p2.X);
            return major != 0 ? major : Y.CompareTo(p2.Y);
        }

        // Twice the signed area of the triangle (p0, p1, p2)
        public static double Area2(ConvexHullPoint p0, ConvexHullPoint p1, ConvexHullPoint p2)
        {
            return p0.X * (p1.Y - p2.Y) + p1.X * (p2.Y - p0.Y) + p2.X * (p0.Y - p1.Y);
        }
    
        // The area of a polygon (represented by an array of ordered vertices)
        public static double Area(ConvexHullPoint[] pts)
        {
            var n = pts.Length;
            var origo = new ConvexHullPoint(0, 0);
            double area2 = 0;
            for (var i = 0; i < n; i++)
                area2 += Area2(origo, pts[i], pts[(i + 1) % n]);
            return Math.Abs(area2 / 2);
        }
    }

}

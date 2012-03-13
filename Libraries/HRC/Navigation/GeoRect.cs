using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Windows;
using HRC.Aspects;

namespace HRC.Navigation
{
    [Serializable, NotifyPropertyChanged]
    public class GeoRect : IEquatable<GeoRect>, IDataErrorInfo
    {
        public GeoRect() { }

        public GeoRect(double north, double south, double east, double west)
        {
            North = north;
            South = south;
            East = east;
            West = west;
        }

        public GeoRect(Rect rect) { FromRect(rect); }

        public GeoRect(GeoRect geoRect) : this(geoRect.North, geoRect.South, geoRect.East, geoRect.West) { }

        public GeoRect(IEnumerable<Geo> geoList)
        {
            North = East = double.MinValue;
            South = West = double.MaxValue;
            foreach (var geo in geoList)
            {
                North = Math.Max(North, geo.Latitude);
                South = Math.Min(South, geo.Latitude);
                East = Math.Max(East, geo.Longitude);
                West = Math.Min(West, geo.Longitude);
            }
        }

        [Affects("NorthEast", "NorthWest", "Center", "HeightKm", "Height")]
        public double North { get; set; }
        [Affects("SouthEast", "SouthWest", "Center", "HeightKm", "Height")]
        public double South { get; set; }
        [Affects("NorthEast", "SouthEast", "Center", "AverageWidthKm", "Width")]
        public double East { get; set; }
        [Affects("NorthWest", "SouthWest", "Center", "AverageWidthKm", "Width")]
        public double West { get; set; }
        public Geo NorthWest { get { return new Geo(North, West); } }
        public Geo NorthEast { get { return new Geo(North, East); } }
        public Geo SouthWest { get { return new Geo(South, West); } }
        public Geo SouthEast { get { return new Geo(South, East); } }
        public Geo Center { get { return new Geo((North + South) / 2, (East + West) / 2); } }
        public double AverageWidthKm { get { return (NorthWest.DistanceKilometers(NorthEast) + SouthWest.DistanceKilometers(SouthEast)) / 2; } }
        public double HeightKm { get { return (NorthWest.DistanceKilometers(SouthWest) + NorthEast.DistanceKilometers(SouthEast)) / 2; } }
        public double Width { get { return East - West; } }
        public double Height { get { return North - South; } }

        /// <summary>
        ///   Expands the current GeoRect exactly enough to contain the specified GeoRect.
        /// </summary>
        /// <param name = "geoRect"></param>
        public void Union(GeoRect geoRect)
        {
            North = Math.Max(North, geoRect.North);
            South = Math.Min(South, geoRect.South);
            East = Math.Max(East, geoRect.East);
            West = Math.Min(West, geoRect.West);
        }

        /// <summary>
        ///   Expands the current GeoRect exactly enough to contain the specified EarthCoordinate.
        /// </summary>
        /// <param name = "geo"></param>
        public void Union(Geo geo)
        {
            North = Math.Max(North, geo.Latitude);
            South = Math.Min(South, geo.Latitude);
            East = Math.Max(East, geo.Longitude);
            West = Math.Min(West, geo.Longitude);
        }

        /// <summary>
        /// Returns an enumerable that will result in a closed, clockwise polygon
        /// NorthWest, NorthEast, SouthEast, SouthWest, NorthWest
        /// </summary>
        public IEnumerable<Geo> ClosedBoundaryCoordinates
        {
            get
            {
                yield return NorthWest;
                yield return NorthEast;
                yield return SouthEast;
                yield return SouthWest;
                yield return NorthWest;
            }
        }

        /// <summary>
        /// Returns a GeoRect that encompasses all the GeoRects passed in as parameters
        /// </summary>
        /// <param name = "geoRects"></param>
        public static GeoRect Union(params GeoRect[] geoRects)
        {
            if (geoRects.Length == 0) return null;
            var result = new GeoRect(geoRects[0]);
            if (geoRects.Length > 1) for (var i = 1; i < geoRects.Length; i++) result.Union(geoRects[i]);
            return result;
        }

        /// <summary>
        ///   Creates a GeoRect that is exactly large enough to include the specified GeoRect and the specified EarthCoordinate.
        /// </summary>
        /// <param name = "geoRect"></param>
        /// <param name = "geo"></param>
        /// <returns></returns>
        public static GeoRect Union(GeoRect geoRect, Geo geo) //{ return new GeoRect(Rect.Union(geoRect, geo)); }
        {
            return new GeoRect(Math.Max(geoRect.North, geo.Latitude),
                               Math.Min(geoRect.South, geo.Latitude),
                               Math.Max(geoRect.East, geo.Longitude),
                               Math.Min(geoRect.West, geo.Longitude));
        }

        /// <summary>
        ///   Indicates whether the GeoRect contains the specified Geo.
        /// </summary>
        /// <param name = "geo"></param>
        /// <returns></returns>
        public bool Contains(Geo geo) { return South <= geo.Latitude && geo.Latitude <= North && West <= geo.Longitude && geo.Longitude <= East; }

        /// <summary>
        ///   Indicates whether the GeoRect contains the specified GeoRect.
        /// </summary>
        /// <param name = "geoRect"></param>
        /// <returns></returns>
        public bool Contains(GeoRect geoRect) { return South <= geoRect.South && geoRect.North <= North && West <= geoRect.West && geoRect.East <= East; }

        /// <summary>
        ///   Indicates whether the specified GeoRect is equal to the current GeoRect.
        /// </summary>
        /// <param name = "geoRect"></param>
        /// <returns></returns>
        public bool Equals(GeoRect geoRect)
        {
            return Math.Abs(North - geoRect.North) < 0.0001 &&
                   Math.Abs(South - geoRect.South) < 0.0001 &&
                   Math.Abs(East - geoRect.East) < 0.0001 &&
                   Math.Abs(West - geoRect.West) < 0.0001;
        }

        /// <summary>
        ///   Indicates whether the specified GeoRects are equal.
        /// </summary>
        /// <param name = "geoRect1"></param>
        /// <param name = "geoRect2"></param>
        /// <returns></returns>
        public static bool Equals(GeoRect geoRect1, GeoRect geoRect2) { return geoRect1.Equals(geoRect2); }

        /// <summary>
        ///   Returns the intersection of the specified GeoRects.
        /// </summary>
        /// <param name = "geoRect1"></param>
        /// <param name = "geoRect2"></param>
        /// <returns></returns>
        public static GeoRect Intersect(GeoRect geoRect1, GeoRect geoRect2)
        {
            return new GeoRect(Math.Min(geoRect1.North, geoRect2.North),
                               Math.Max(geoRect1.South, geoRect2.South),
                               Math.Min(geoRect1.East, geoRect2.East),
                               Math.Max(geoRect1.West, geoRect2.West));
        }

        /// <summary>
        ///   Creates a GeoRect that results from expanding or shrinking the specified GeoRect by the specified width and height amounts, in all directions.
        /// </summary>
        /// <param name = "geoRect">The GeoRect to expand or shrink</param>
        /// <param name = "width">Amount to change the width of the GeoRect, in meters</param>
        /// <param name = "height">Amount to change the height of the GeoRect, in meters</param>
        /// <returns></returns>
        public static GeoRect Inflate(GeoRect geoRect, double width, double height)
        {
            var newNorthWest = new Geo(geoRect.NorthWest);
            newNorthWest.Move(0, height);
            newNorthWest.Move(270, width);

            var newSouthEast = new Geo(geoRect.SouthEast);
            newSouthEast.Move(180, height);
            newSouthEast.Move(90, width);

            return new GeoRect(newNorthWest.Latitude, newSouthEast.Latitude, newSouthEast.Longitude, newNorthWest.Longitude);
        }

        public static GeoRect Inflate(GeoRect geoRect, double rangeOutKm)
        {
            var northWest = Geo.FromDegrees(geoRect.North, geoRect.West).Offset(Geo.KilometersToRadians(Math.Sqrt(2) * rangeOutKm), Geo.DegreesToRadians(315));
            var southEast = Geo.FromDegrees(geoRect.South, geoRect.East).Offset(Geo.KilometersToRadians(Math.Sqrt(2) * rangeOutKm), Geo.DegreesToRadians(135));
            return new GeoRect(northWest.Latitude, southEast.Latitude, southEast.Longitude, northWest.Longitude);
        }

        void FromRect(Rect rect)
        {
            North = rect.Bottom;
            South = rect.Top;
            East = rect.Right;
            West = rect.Left;
        }

        public static explicit operator Rect(GeoRect geoRect) { return new Rect(geoRect.West, geoRect.South, geoRect.Width, geoRect.Height); }

        public string this[string columnName] { get { return Error; } }

        public string Error
        {
            get
            {
                return North >= 0 && North <= 90 &&
                       South >= 0 && South <= 90 && North >= South &&
                       West >= -360 && West <= 360 &&
                       East >= 360 && East <= 360 && East >= West
                           ? null
                           : string.Format("Poorly-formed GeoRect: North={0} South={1} East={2} West={3}", North, South, East, West);
            }
        }
    }
}
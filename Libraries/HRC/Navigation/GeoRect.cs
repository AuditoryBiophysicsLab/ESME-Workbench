using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Windows;
using System.Xml.Serialization;
using Cinch;
using HRC.Utility;

namespace HRC.Navigation
{
    [Serializable]
    public class GeoRect : PropertyChangedBase, IEquatable<GeoRect>
    {
        public GeoRect() { North = South = East = West = 0.0; }

        public GeoRect(double north, double south, double east, double west)
        {
            North = north;
            South = south;
            East = east;
            West = west;
        }

        public GeoRect(Rect rect) { FromRect(rect); }

        public GeoRect(GeoRect geoRect)
        {
            North = geoRect.North;
            South = geoRect.South;
            East = geoRect.East;
            West = geoRect.West;
        }

        public GeoRect(IEnumerable<Geo> geoList)
        {
            North = double.MinValue;
            South = double.MaxValue;
            East = double.MinValue;
            West = double.MaxValue;
            foreach (var geo in geoList)
            {
                North = Math.Max(North, geo.Latitude);
                South = Math.Min(South, geo.Latitude);
                East = Math.Max(East, geo.Longitude);
                West = Math.Min(West, geo.Longitude);
            }
        }

        #region public double North { get; set; }

        /// <summary>
        ///   The North edge of the GeoRect
        /// </summary>
        public double North
        {
            get { return _north; }
            set
            {
                if (Math.Abs(_north - value) < 0.0001) return;
                _north = value;
                NotifyPropertyChanged(NorthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NorthChangedEventArgs = ObservableHelper.CreateArgs<GeoRect>(x => x.North);
        double _north;

        #endregion

        #region public double South { get; set; }

        /// <summary>
        ///   The South edge of the GeoRect
        /// </summary>
        public double South
        {
            get { return _south; }
            set
            {
                if (Math.Abs(_south - value) < 0.0001) return;
                _south = value;
                NotifyPropertyChanged(SouthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SouthChangedEventArgs = ObservableHelper.CreateArgs<GeoRect>(x => x.South);
        double _south;

        #endregion

        #region public double East { get; set; }

        /// <summary>
        ///   The East edge of the GeoRect
        /// </summary>
        public double East
        {
            get { return _east; }
            set
            {
                if (Math.Abs(_east - value) < 0.0001) return;
                _east = value;
                NotifyPropertyChanged(EastChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs EastChangedEventArgs = ObservableHelper.CreateArgs<GeoRect>(x => x.East);
        double _east;

        #endregion

        #region public double West { get; set; }

        /// <summary>
        ///   The West edge of the GeoRect
        /// </summary>
        public double West
        {
            get { return _west; }
            set
            {
                if (Math.Abs(_west - value) < 0.0001) return;
                _west = value;
                NotifyPropertyChanged(WestChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WestChangedEventArgs = ObservableHelper.CreateArgs<GeoRect>(x => x.West);
        double _west;

        #endregion

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
        ///   Expands the current GeoRect exactly enough to contain the specified rectangle.
        /// </summary>
        /// <param name = "rect"></param>
        public void Union(Rect rect) { FromRect(Rect.Union(this, rect)); }

        /// <summary>
        ///   Expands the current GeoRect exactly enough to contain the specified EarthCoordinate.
        /// </summary>
        /// <param name = "geo"></param>
        public void Union(Geo geo) //{ FromRect(Rect.Union(this, geo)); }
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
        public bool Contains(Geo geo)
        {
            return North <= geo.Latitude && geo.Latitude <= South && West <= geo.Longitude && geo.Longitude <= East;
        }

        /// <summary>
        ///   Indicates whether the GeoRect contains the specified GeoRect.
        /// </summary>
        /// <param name = "geoRect"></param>
        /// <returns></returns>
        public bool Contains(GeoRect geoRect) { return ((Rect) this).Contains(geoRect); }

        /// <summary>
        ///   Indicates whether the specified GeoRect is equal to the current GeoRect.
        /// </summary>
        /// <param name = "geoRect"></param>
        /// <returns></returns>
        public bool Equals(GeoRect geoRect) { return ((Rect) this).Equals(geoRect); }

        /// <summary>
        ///   Indicates whether the specified GeoRects are equal.
        /// </summary>
        /// <param name = "geoRect1"></param>
        /// <param name = "geoRect2"></param>
        /// <returns></returns>
        public static bool Equals(GeoRect geoRect1, GeoRect geoRect2) { return Rect.Equals(geoRect1, geoRect2); }

        /// <summary>
        ///   Returns the intersection of the specified GeoRects.
        /// </summary>
        /// <param name = "geoRect1"></param>
        /// <param name = "geoRect2"></param>
        /// <returns></returns>
        public static GeoRect Intersect(GeoRect geoRect1, GeoRect geoRect2) { return new GeoRect(Rect.Intersect(geoRect1, geoRect2)); }

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

        [XmlIgnore]
        public Geo NorthWest
        {
            get { return new Geo(North, West); }
        }

        [XmlIgnore]
        public Geo NorthEast
        {
            get { return new Geo(North, East); }
        }

        [XmlIgnore]
        public Geo SouthWest
        {
            get { return new Geo(South, West); }
        }

        [XmlIgnore]
        public Geo SouthEast
        {
            get { return new Geo(South, East); }
        }

        [XmlIgnore]
        public Geo Center
        {
            get { return new Geo((North + South) / 2, (East + West) / 2); }
        }

        [XmlIgnore]
        public double AverageWidthKm
        {
            get { return (NorthWest.DistanceKilometers(NorthEast) + SouthWest.DistanceKilometers(SouthEast)) / 2; }
        }

        [XmlIgnore]
        public double HeightKm
        {
            get { return (NorthWest.DistanceKilometers(SouthWest) + NorthEast.DistanceKilometers(SouthEast)) / 2; }
        }

        [XmlIgnore]
        public double Width
        {
            get { return East - West; }
        }

        [XmlIgnore]
        public double Height
        {
            get { return North - South; }
        }

        public static implicit operator Rect(GeoRect geoRect) { return new Rect(geoRect.West, geoRect.South, geoRect.Width, geoRect.Height); }

        void FromRect(Rect rect)
        {
            North = rect.Bottom;
            South = rect.Top;
            East = rect.Right;
            West = rect.Left;
        }
    }
}
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

        public GeoRect(GeoRect geoRect) { FromRect(geoRect); }

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
                if (_north == value) return;
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
                if (_south == value) return;
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
                if (_east == value) return;
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
                if (_west == value) return;
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
        public void Union(GeoRect geoRect) { FromRect(Rect.Union(this, geoRect)); }

        /// <summary>
        ///   Expands the current GeoRect exactly enough to contain the specified rectangle.
        /// </summary>
        /// <param name = "rect"></param>
        public void Union(Rect rect) { FromRect(Rect.Union(this, rect)); }

        /// <summary>
        ///   Expands the current GeoRect exactly enough to contain the specified EarthCoordinate.
        /// </summary>
        /// <param name = "earthCoordinate"></param>
        public void Union(EarthCoordinate earthCoordinate) { FromRect(Rect.Union(this, earthCoordinate)); }

        /// <summary>
        /// Returns an enumerable that will result in a closed, clockwise polygon
        /// NorthWest, NorthEast, SouthEast, SouthWest, NorthWest
        /// </summary>
        public IEnumerable<EarthCoordinate> ClosedBoundaryCoordinates
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
        ///   Creates a GeoRect that is exactly large enough to contain the two specified GeoRects.
        /// </summary>
        /// <param name = "geoRect1"></param>
        /// <param name = "geoRect2"></param>
        /// <returns></returns>
        public static GeoRect Union(GeoRect geoRect1, GeoRect geoRect2) { return new GeoRect(Rect.Union(geoRect1, geoRect2)); }

        /// <summary>
        ///   Creates a GeoRect that is exactly large enough to include the specified GeoRect and the specified EarthCoordinate.
        /// </summary>
        /// <param name = "geoRect"></param>
        /// <param name = "earthCoordinate"></param>
        /// <returns></returns>
        public static GeoRect Union(GeoRect geoRect, EarthCoordinate earthCoordinate) { return new GeoRect(Rect.Union(geoRect, earthCoordinate)); }

        /// <summary>
        ///   Indicates whether the GeoRect contains the specified EarthCoordinate.
        /// </summary>
        /// <param name = "earthCoordinate"></param>
        /// <returns></returns>
        public bool Contains(EarthCoordinate earthCoordinate) { return ((Rect) this).Contains(earthCoordinate); }

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
            var newNorthWest = new EarthCoordinate(geoRect.NorthWest);
            newNorthWest.Move(0, height);
            newNorthWest.Move(270, width);

            var newSouthEast = new EarthCoordinate(geoRect.SouthEast);
            newSouthEast.Move(180, height);
            newSouthEast.Move(90, width);

            return new GeoRect(newNorthWest.Latitude, newSouthEast.Latitude, newSouthEast.Longitude, newNorthWest.Longitude);
        }

        public static GeoRect InflateWithGeo(GeoRect geoRect, double rangeOutKm)
        {
            var northWest = Geo.FromDegrees(geoRect.North, geoRect.West).Offset(Geo.KilometersToRadians(Math.Sqrt(2) * rangeOutKm), Geo.DegreesToRadians(315));
            var northEast = Geo.FromDegrees(geoRect.North, geoRect.East).Offset(Geo.KilometersToRadians(Math.Sqrt(2) * rangeOutKm), Geo.DegreesToRadians(45));
            var southEast = Geo.FromDegrees(geoRect.South, geoRect.East).Offset(Geo.KilometersToRadians(Math.Sqrt(2) * rangeOutKm), Geo.DegreesToRadians(135));
            var southWest = Geo.FromDegrees(geoRect.South, geoRect.West).Offset(Geo.KilometersToRadians(Math.Sqrt(2) * rangeOutKm), Geo.DegreesToRadians(225));
            return new GeoRect(northWest.Latitude, southEast.Latitude, southEast.Longitude, northWest.Longitude);
        }

        [XmlIgnore]
        public EarthCoordinate NorthWest
        {
            get { return new EarthCoordinate(North, West); }
        }

        [XmlIgnore]
        public EarthCoordinate NorthEast
        {
            get { return new EarthCoordinate(North, East); }
        }

        [XmlIgnore]
        public EarthCoordinate SouthWest
        {
            get { return new EarthCoordinate(South, West); }
        }

        [XmlIgnore]
        public EarthCoordinate SouthEast
        {
            get { return new EarthCoordinate(South, East); }
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
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Windows.Media;
using ESME.Overlay;
using HRC.Navigation;
using FileFormatException = ESME.Model.FileFormatException;

namespace ESME.Environment
{
#if false
    public abstract class Environment2DData<TCoordinate> : GenericGeoField<TCoordinate>
        where TCoordinate : EarthCoordinate
    {
        protected Environment2DData(TCoordinate[,] data) : base(data) { }
        protected Environment2DData(IEnumerable<TCoordinate> data) : base(data) { }

        #region Public properties

        public OverlayLineSegments BoundingBox
        {
            get
            {
                var geoRect = GeoRect;
                var bathyBox = new[]
                               {
                                   new EarthCoordinate(geoRect.North, geoRect.East), // northeast corner
                                   new EarthCoordinate(geoRect.South, geoRect.East), // southeast corner
                                   new EarthCoordinate(geoRect.South, geoRect.West), // southwest corner
                                   new EarthCoordinate(geoRect.North, geoRect.West), // northwest corner
                                   new EarthCoordinate(geoRect.North, geoRect.East), // northeast corner again to close the loop.
                               };

                var shape = new OverlayLineSegments(bathyBox, Colors.Black, 1, LineStyle.Solid);
                return shape;
            }
        }

        /// <summary>
        /// Look up a value in the current data set.
        /// </summary>
        /// <param name="coordinate">
        /// The coordinate to search the data set for
        /// </param>
        /// <param name="value">
        /// The value at the requested coordinate
        /// </param>
        /// <returns>
        /// If the requested coordinate is contained in the data set, the function return value is 'true', 'false' otherwise
        /// </returns>
        public virtual bool Lookup(EarthCoordinate coordinate, out TCoordinate value)
        {
            value = null;
            if (Contains(coordinate))
            {
                var latIndex = Latitudes.IndexOf(coordinate.Latitude);
                var lonIndex = Longitudes.IndexOf(coordinate.Longitude);
                if ((latIndex >= 0) && (lonIndex >= 0))
                {
                    value = FieldData[latIndex, lonIndex];
                    return true;
                }
            }
            return false;
        }

        internal class Tester<T> : IComparable<Tester<T>>
        {
            public T Coordinate { get; set; }
            public double Distance { get; set; }
            public int CompareTo(Tester<T> other) { return Distance.CompareTo(other.Distance); }
            public override string ToString() { return string.Format("{0} : {1}", Coordinate, Distance); }
        }

        public virtual bool ClosestTo(EarthCoordinate coordinate, out TCoordinate value)
        {
            value = null;
            if (Contains(coordinate))
            {
                var result = new List<Tester<TCoordinate>>();
                for (var lon = 0; lon < FieldData.GetLength(0); lon++)
                    for (var lat = 0; lat < FieldData.GetLength(1); lat++)
                        result.Add(new Tester<TCoordinate>
                                   {
                                       Coordinate = FieldData[lon, lat],
                                       Distance = FieldData[lon, lat].DistanceTo(coordinate)
                                   });
                if (result.Count == 0) return false;
                value = result.Min().Coordinate;
                return true;
            }
            return false;
        }

        #endregion
    }

    public class Environment2DData : Environment2DData<EarthCoordinate<float>>
    {
        public string Filename { get; protected set; }

        public Environment2DData(EarthCoordinate<float>[,] data) : base(data) { }
        public Environment2DData(IEnumerable<EarthCoordinate<float>> data) : base(data) { FillTheDamnHoles(); }

        public EarthCoordinate<float> Minimum
        {
            get
            {
                EarthCoordinate<float> curMinPoint = null;
                foreach (var datum in FieldData)
                {
                    if (curMinPoint == null) curMinPoint = datum;
                    else if (datum.Data < curMinPoint.Data) curMinPoint = datum;
                }
                return curMinPoint;
            }
        }

        public EarthCoordinate<float> Maximum
        {
            get
            {
                EarthCoordinate<float> curMaxPoint = null;
                foreach (var datum in FieldData)
                {
                    if (curMaxPoint == null) curMaxPoint = datum;
                    else if (datum.Data > curMaxPoint.Data) curMaxPoint = datum;
                }
                return curMaxPoint;
            }
        }

        private void FillTheDamnHoles()
        {
            var thereIsAtLeastOneHole = true;
            var passCount = 0;
            while (thereIsAtLeastOneHole)
            {
                thereIsAtLeastOneHole = false;
                for (var lat = 0; lat < FieldData.GetLength(1); lat++)
                {
                    for (var lon = 0; lon < FieldData.GetLength(0); lon++)
                    {
                        if (FieldData[lon, lat] == null)
                            if (!FillOneDamnHole(lon, lat))
                                thereIsAtLeastOneHole = true;
                    }
                }
                passCount++;
            }
            System.Diagnostics.Debug.WriteLine("FillTheDamnHoles: Sample holes filled after " + passCount + " passes.");
        }


        private bool FillOneDamnHole(int lonIndex, int latIndex)
        {
            float nearbyValue;
            var possibleValues = new List<float>();

            // See if there's a value to the south, north, west and east first.  Those are the closest to the current point.
            if (TryGetValue(lonIndex - 1, latIndex, out nearbyValue)) possibleValues.Add(nearbyValue);
            if (TryGetValue(lonIndex + 1, latIndex, out nearbyValue)) possibleValues.Add(nearbyValue);
            if (TryGetValue(lonIndex, latIndex - 1, out nearbyValue)) possibleValues.Add(nearbyValue);
            if (TryGetValue(lonIndex, latIndex + 1, out nearbyValue)) possibleValues.Add(nearbyValue);
            if (possibleValues.Count > 0)
            {
                var actualValues = from value in possibleValues
                                   where value >= 0
                                   select value;

                if (actualValues.Count() > 0)
                {
                    FieldData[lonIndex, latIndex] = new EarthCoordinate<float>(Latitudes[latIndex], Longitudes[lonIndex], actualValues.First());
                    return true;
                }
            }

            // If nothing was found above, try the southwest, southeast, northwest and northeast corners
            if (TryGetValue(lonIndex - 1, latIndex - 1, out nearbyValue)) possibleValues.Add(nearbyValue);
            if (TryGetValue(lonIndex - 1, latIndex + 1, out nearbyValue)) possibleValues.Add(nearbyValue);
            if (TryGetValue(lonIndex - 1, latIndex - 1, out nearbyValue)) possibleValues.Add(nearbyValue);
            if (TryGetValue(lonIndex + 1, latIndex + 1, out nearbyValue)) possibleValues.Add(nearbyValue);
            if (possibleValues.Count > 0)
            {
                var actualValues = from value in possibleValues
                                   where value >= 0
                                   select value;

                if (actualValues.Count() > 0)
                {
                    FieldData[lonIndex, latIndex] = new EarthCoordinate<float>(Longitudes[lonIndex], Latitudes[latIndex], actualValues.First());
                    return true;
                }
            }
            // If no values were found, then don't put anything in the current cell, for now.
            return false;
        }

        private bool TryGetValue(int lonIndex, int latIndex, out float value)
        {
            value = 0;
            if ((lonIndex < 0) || (lonIndex >= FieldData.GetLength(0))) return false;
            if ((latIndex < 0) || (latIndex >= FieldData.GetLength(1))) return false;

            if (FieldData[lonIndex, latIndex] == null) return false;
            value = FieldData[lonIndex, latIndex].Data;
            return true;
        }


        public static Environment2DData FromCHB(string fileName, float scaleFactor)
        {
            using (var stream = new BinaryReader(File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)))
            {
                var west = stream.ReadSingle();
                var east = stream.ReadSingle();
                var south = stream.ReadSingle();
                var north = stream.ReadSingle();
                var gridSpacing = stream.ReadSingle() / 60f; // Source is in minutes, we need degrees
                var width = stream.ReadInt32();
                var height = stream.ReadInt32();
                var endian = stream.ReadUInt32();
                if (endian != 0x00010203) throw new FileFormatException("Invalid CHRTR Binary file format - endian is incorrect");
                var maxDepth = stream.ReadSingle() * scaleFactor;
                var minDepth = stream.ReadSingle() * scaleFactor;
                var paddingWidth = (width - 10) * 4;
                stream.ReadBytes(paddingWidth);
                //var depths = new float[height, width];
                var depths = new List<EarthCoordinate<float>>();
                var fieldData = new EarthCoordinate<float>[width, height];
                for (var lat = 0; lat < height; lat++)
                    for (var lon = 0; lon < width; lon++)
                    {
                        var curSample = stream.ReadSingle();
                        var pt = new EarthCoordinate<float>(south + (gridSpacing * lat), west + (gridSpacing * lon), curSample == 1e16f ? float.NaN : curSample * scaleFactor);
                        //depths.Add(pt);
                        fieldData[lon, lat] = pt;

                        //if ((curSample > 999) || (curSample < 1)) 
                        //    System.Diagnostics.Debugger.Break();
                        //depths[lat, lon] = curSample == 1e16f ? float.NaN : curSample * scaleFactor;
                    }
                return new Environment2DData(fieldData);
                //return new Environment2DData(depths);
                //return new Environment2DData(north, south, east, west, gridSpacing, depths, minDepth, maxDepth);
            }
        }

        public static Environment2DData FromYXZ(string fileName, float scaleFactor)
        {
            char[] separators = {' '};
            using (var stream = new StreamReader(File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)))
            {
                var fieldData = new List<EarthCoordinate<float>>();
                var curLine = stream.ReadLine();
                while (curLine != null)
                {
                    var fields = curLine.Split(separators, StringSplitOptions.RemoveEmptyEntries);
                    fieldData.Add(new EarthCoordinate<float>(double.Parse(fields[0]), double.Parse(fields[1]), float.Parse(fields[2]) * scaleFactor));
                    curLine = stream.ReadLine();
                }
                return new Environment2DData(fieldData);
            }
        }

        /// <summary>
        /// Saves a data set to a text file in YXZ format.
        /// The output data file will be a three-column text file 
        /// LatitudeValue LongitudeValue ScaleFactor*DataValueAtLatLon
        /// </summary>
        /// <param name="fileName"></param>
        /// <param name="scaleFactor"></param>
        public void SaveToYXZ(string fileName, float scaleFactor)
        {
            using (var stream = new StreamWriter(File.Create(fileName)))
            {
                for (var lat = 0; lat < Latitudes.Count; lat++)
                    for (var lon = 0; lon < Longitudes.Count; lon++)
                        stream.WriteLine(string.Format("{0:##.####} {1:###.####} {2:#.###}", Latitudes[lat], Longitudes[lon], scaleFactor * FieldData[lon, lat].Data));
            }
        }
    }
#endif
}
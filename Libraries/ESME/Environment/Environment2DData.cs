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
    public abstract class Environment2DData<TCoordinate> : GenericGeoField<TCoordinate>
        where TCoordinate : EarthCoordinate
    {
        protected Environment2DData(TCoordinate[,] data) : base(data) { }
        protected Environment2DData(IEnumerable<TCoordinate> data) : base(data) { }

        public void Save(string filename)
        {
            using (var stream = new BinaryWriter(new FileStream(filename, FileMode.Create)))
            {
                Save(stream);
            }
        }

        public abstract void Save(BinaryWriter stream);
        #region Public properties

        public OverlayLineSegments BoundingBox
        {
            get
            {
                var geoRect = GeoRect;
                var bathyBox = new[]
                               {
                                   //edit: Modified this routine to take the horizontal and vertical resolution into account
                                   //      It now places the bounding box such that the lines are coincident with the edges of
                                   //      the edge samples of the selected data (extends by half the horizontal/vertical resolution)
                                   //northeast corner:                   
                                   //new EarthCoordinate(geoRect.North + (LatitudinalResolution/2), geoRect.East + (LongitudinalResolution/2)), // northeast corner
                                   //new EarthCoordinate(geoRect.South - (LatitudinalResolution/2), geoRect.East + (LongitudinalResolution/2)), // southeast corner                                   new EarthCoordinate(SouthWest.Latitude - (LatitudinalResolution/2), SouthWest.Longitude - (LongitudinalResolution/2)), // southwest corner: 
                                   //new EarthCoordinate(geoRect.South - (LatitudinalResolution/2), geoRect.West - (LongitudinalResolution/2)), // southwest corner                                   new EarthCoordinate(SouthWest.Latitude - (LatitudinalResolution/2), SouthWest.Longitude - (LongitudinalResolution/2)), // southwest corner: 
                                   //new EarthCoordinate(geoRect.North + (LatitudinalResolution/2), geoRect.West - (LongitudinalResolution/2)), // northwest corner
                                   //new EarthCoordinate(geoRect.North + (LatitudinalResolution/2), geoRect.East + (LongitudinalResolution/2)), // northeast corner again to close the loop.
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
        #region Public properties

        public string Filename { get; protected set; }

        #endregion

        const UInt32 Magic = 0x748dcde6;

        #region Public constructors

        public Environment2DData(EarthCoordinate<float>[,] data) : base(data)
        {
            
        }
        public Environment2DData(IEnumerable<EarthCoordinate<float>> data)
            : base(data)
        {
            FillTheDamnHoles();
        }

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

#if false
        // Deprecated on 14 Feb 2011 by Dave Anderson
        public Environment2DData(double north, double south, double east, double west, float gridSpacing, float[,] values, float minValue, float maxValue)
        {
            MinCoordinate = new EarthCoordinate(south, west);
            MaxCoordinate = new EarthCoordinate(north, east);
            MinValue = minValue;
            MaxValue = maxValue;
            for (var lon = 0; lon < values.GetLength(1); lon++) Longitudes.Add(west + (lon * gridSpacing));
            for (var lat = 0; lat < values.GetLength(0); lat++) Latitudes.Add(south + (lat * gridSpacing));
            FieldData = values;
        }
#endif

        public static Environment2DData FromEEB(string fileName, string layerName, GeoRect extractionArea)
        {
            if (Path.GetExtension(fileName) != ".eeb") throw new System.IO.FileFormatException(string.Format("Environment2DData: Unknown file type \"{0}\"", Path.GetFileName(fileName)));
            var file = DataFile.Open(fileName);

            var layer = file[layerName];
            if (layer == null) throw new System.IO.FileFormatException(string.Format("Environment2DData: Specified environment file \"{0}\"does not contain a environment2DData layer", fileName));

            var data = new List<EarthCoordinate<float>>();
            foreach (var row in layer.GetRows((float)extractionArea.South, (float)extractionArea.North))
                data.AddRange(from point in row.Points
                              where (point.EarthCoordinate.Longitude >= extractionArea.West) && (point.EarthCoordinate.Longitude <= extractionArea.East)
                              select new EarthCoordinate<float>(point.EarthCoordinate.Latitude, point.EarthCoordinate.Longitude, point.Data[0]));
            return new Environment2DData(data)
                   {
                       Filename = fileName
                   };
#if false
            Longitudes.AddRange(layer.LongitudeAxis.DoubleValuesBetween(west, east));
            Latitudes.AddRange(layer.LatitudeAxis.DoubleValuesBetween(south, north));
            int[] lonIndices = layer.LongitudeAxis.IndicesBetween(west, east);
            int[] latIndices = layer.LatitudeAxis.IndicesBetween(south, north);

            MinCoordinate = new EarthCoordinate(Latitudes[0], Longitudes[0]);
            MaxCoordinate = new EarthCoordinate(Latitudes[Latitudes.Count - 1], Longitudes[Longitudes.Count - 1]);
            MinValue = float.MaxValue;
            MaxValue = float.MinValue;
            Values = layer.Get2DData(latIndices[0], latIndices[latIndices.Length - 1], lonIndices[0], lonIndices[lonIndices.Length - 1]);
            for (var row = 0; row < Values.GetLength(0); row++)
                for (var col = 0; col < Values.GetLength(1); col++)
                {
                    MinValue = Math.Min(MinValue, Values[row, col]);
                    MaxValue = Math.Max(MaxValue, Values[row, col]);
                }
#endif
        }

#if false
        public Environment2DData(string fileName)
        {
            Filename = fileName;
            if (Path.GetExtension(fileName) == ".eeb")
            {
                //float[, ,] array;
                //float curValue;
                DataFile file = DataFile.Open(fileName);
                foreach (DataLayer layer in file.Layers)
                {
                    if (layer.Name != "environment2DData") continue;
                    Latitudes.AddRange(layer.LatitudeAxis.UnwrappedValues);
                    Longitudes.AddRange(layer.LongitudeAxis.UnwrappedValues);
                    //array = layer.DataArray.Data;

                    MinCoordinate = new EarthCoordinate(Latitudes[0], Longitudes[0]);
                    MaxCoordinate = new EarthCoordinate(Latitudes[Latitudes.Count - 1], Longitudes[Longitudes.Count - 1]);

                    MinValue = float.MaxValue;
                    MaxValue = float.MinValue;

                    Values = layer.Get2DData(0, layer.RowCount - 1, layer.RowCount, 0, layer.ColumnCount - 1, layer.ColumnCount);
                    for (int row = 0; row < Values.GetLength(0); row++)
                        for (int col = 0; col < Values.GetLength(1); col++)
                        {
                            MinValue = Math.Min(MinValue, Values[row, col]);
                            MaxValue = Math.Max(MaxValue, Values[row, col]);
                        }
                }
            }
            else
            {
                using (var stream = new BinaryReader(new FileStream(fileName, FileMode.Open)))
                {
                    Load(stream);
                }
            }
            Filename = fileName;
        }
#endif

        #endregion

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

        public static Environment2DData Load(BinaryReader stream)
        {
            if (stream.ReadUInt32() != Magic) throw new FormatException("Attempted to read invalid data into environment2DData");
            var data = new List<EarthCoordinate<float>>();

            var lonCount = stream.ReadInt32();
            var latCount = stream.ReadInt32();

            for (var lat = 0; lat < latCount; lat++)
                for (var lon = 0; lon < lonCount; lon++)
                    data.Add(new EarthCoordinate<float>(stream.ReadDouble(), stream.ReadDouble(), stream.ReadSingle()));
            return new Environment2DData(data);
        }

        public override void Save(BinaryWriter stream)
        {
            stream.Write(Magic);

            stream.Write(Longitudes.Count);
            stream.Write(Latitudes.Count);

            for (var lon = 0; lon < Longitudes.Count; lon++)
                for (var lat = 0; lat < Latitudes.Count; lat++)
                {
                    stream.Write(FieldData[lon, lat].Latitude);
                    stream.Write(FieldData[lon, lat].Longitude);
                    stream.Write(FieldData[lon, lat].Data);
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

    public class AverageDatum
    {
        public AverageDatum()
        {
            Value = 0;
            Count = 0;
        }

        public AverageDatum(float value)
        {
            Value = value;
            Count = 1;
        }

        public float Value { get; set; }
        public int Count { get; private set; }

        public void Add(float newValue)
        {
            Value += newValue;
            Count++;
        }

        public float Average { get { return Value / Count; } }
    }
#if false
    public sealed class Environment3DAverager : Environment2DData<EarthCoordinate<List<AverageDatum>>>
    {
        public Environment3DAverager(IEnumerable<double> depths, IEnumerable<EarthCoordinate<List<AverageDatum>>> data)
            : base(data)
        {
            Depths = new List<double>();
            Depths.AddRange(depths.Distinct());
            Depths.Sort();
        }

        public List<double> Depths { get; internal set; }

        internal void Add(Environment3DAverager that)
        {
            // Verify that this.Latitudes[] that.Latitudes[] (same with Longitudes[]) are the same length AND have the same contents
            // use VerifyArrays, below
            // Same thing with Depths[]
            // Loop through all the Values[,] and make sure that Count is equal to the corresponding Count of the 'that' data
            // If all checks pass, then just loop through Values[,] as above and add (+=) the corresponding Values from 'that'

            VerifyArrays(Latitudes, that.Latitudes, "latitude");
            VerifyArrays(Longitudes, that.Longitudes, "longitude");
            // Make sure the Depths array is copied from the longest Depths array we are presented with
            if (Depths.Count < that.Depths.Count)
            {
                that.Depths.Clear();
                that.Depths.AddRange(Depths);
            }
            //VerifyArrays(Depths, that.Depths, "depth");)

            for (var lonIndex = 0; lonIndex < FieldData.GetLength(0); lonIndex++)
                for (var latIndex = 0; latIndex < FieldData.GetLength(1); latIndex++)
                {
                    if (FieldData[lonIndex, latIndex] == null) FieldData[lonIndex, latIndex].Data = new List<AverageDatum>();
                    for (var depthIndex = 0; depthIndex < that.FieldData[lonIndex, latIndex].Data.Count; depthIndex++)
                    {
                        var thatValue = that.FieldData[lonIndex, latIndex].Data[depthIndex].Value;
                        if (FieldData[lonIndex, latIndex].Data.Count <= depthIndex) FieldData[lonIndex, latIndex].Data.Add(new AverageDatum(thatValue));
                        else FieldData[lonIndex, latIndex].Data[depthIndex].Add(thatValue);
                    }
                }
        }

        static void VerifyArrays(ICollection<double> left, IList<double> right, string name)
        {
            if (left.Count != right.Count) throw new ApplicationException(name + " array length mismatch");
            if (left.Where((t, i) => t != right[i]).Any()) throw new ApplicationException(name + " array element value mismatch");
        }

        internal void Average()
        {
            for (var lonIndex = 0; lonIndex < FieldData.GetLength(0); lonIndex++)
                for (var latIndex = 0; latIndex < FieldData.GetLength(1); latIndex++)
                {
                    for (var depthIndex = 0; depthIndex < FieldData[lonIndex, latIndex].Data.Count; depthIndex++)
                    {
                        FieldData[lonIndex, latIndex].Data[depthIndex].Average();
                    }
                }
        }

        public override void Save(BinaryWriter stream) { throw new NotImplementedException(); }
        public static void Load(BinaryReader stream) { throw new NotImplementedException(); }
    }

    public sealed class Environment3DData : Environment2DData<EarthCoordinate<List<double>>>
    {
        const UInt32 Magic = 0x3dadcde6;

        public List<double> Depths { get; set; }
#if false
        public static Environment3DData Load(BinaryReader stream)
        {
            if (stream.ReadUInt32() != Magic) throw new FormatException("Attempted to read invalid data into environment3DData");

            MinCoordinate = new EarthCoordinate(stream);
            MaxCoordinate = new EarthCoordinate(stream);

            var lonCount = stream.ReadInt32();
            for (var lon = 0; lon < lonCount; lon++) Longitudes.Add(stream.ReadDouble());

            var latCount = stream.ReadInt32();
            for (var lat = 0; lat < latCount; lat++) Latitudes.Add(stream.ReadDouble());

            var depCount = stream.ReadInt32();
            for (var dep = 0; dep < depCount; dep++) Depths.Add(stream.ReadDouble());

            for (var lat = 0; lat < Latitudes.Count; lat++)
                for (var lon = 0; lon < Longitudes.Count; lon++)
                {
                    var curData = Depths.Select(t => (double)stream.ReadSingle()).Where(curValue => !double.IsNaN(curValue)).ToList();
                    Values[lat, lon] = curData;
                }
        }

        public override void Save(BinaryWriter stream)
        {
            stream.Write(Magic);

            MinCoordinate.Write(stream);
            MaxCoordinate.Write(stream);

            stream.Write(Longitudes.Count);
            foreach (var lon in Longitudes) stream.Write(lon);

            stream.Write(Latitudes.Count);
            foreach (var lat in Latitudes) stream.Write(lat);

            stream.Write(Depths.Count);
            foreach (var dep in Depths) stream.Write(dep);

            for (var lat = 0; lat < Latitudes.Count; lat++)
                for (var lon = 0; lon < Longitudes.Count; lon++)
                {
                    var curValue = Values[lat, lon];
                    for (var dep = 0; dep < Depths.Count; dep++) 
                        stream.Write(curValue.Count < dep ? curValue[dep] : float.NaN);
                }
        }
#endif
        public override void Save(BinaryWriter stream)
        {
            throw new NotImplementedException();
        }

        #region Public constructors

        public Environment3DData(IEnumerable<double> depths, IEnumerable<EarthCoordinate<List<double>>> data)
            : base(data)
        {
            Depths = new List<double>();
            Depths.AddRange(depths.Distinct());
            Depths.Sort();
        }

#if false
        public Environment3DData(double north, double south, double east, double west, float gridSpacing, IEnumerable<double> depths, List<double>[,] values) 
        {
            MinCoordinate = new EarthCoordinate(south, west);
            MaxCoordinate = new EarthCoordinate(north, east);
            Depths = new List<double>();
            for (var lon = 0; lon < values.GetLength(0); lon++) Longitudes.Add(west + (lon * gridSpacing));
            for (var lat = 0; lat < values.GetLength(1); lat++) Latitudes.Add(south + (lat * gridSpacing));
            Depths.AddRange(depths);
            Values = values;
        }
#endif

        #endregion
    }
#endif

}
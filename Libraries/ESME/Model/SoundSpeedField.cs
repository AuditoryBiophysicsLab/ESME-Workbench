using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Xml.Serialization;
using ESME.Environment;
using ESME.Environment.NAVO;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Model
{
#if false
    public class EnvironmentDataPoint<T> : EarthCoordinate
    {
        public T Value { get; set; }
    }

    public class DepthValueList : EarthCoordinate
    {
        public List<float> Depths { get; set; }
        public List<float> Values { get; set; }
        public DepthValueList() { }
        public DepthValueList(IEnumerable<float> depths, IEnumerable<float> values)
        {
            if (depths.Count() < values.Count()) throw new IndexOutOfRangeException("DepthValueList: Depth array must be of equal or greater length than Value array");
            Depths = new List<float>();
            Depths.AddRange(depths);
            Values = new List<float>();
            Values.AddRange(values);
            if (Depths.Count > Values.Count) 
                Depths.RemoveRange(Values.Count, Depths.Count - Values.Count);
        }

        public float MaxDepth
        {
            get { return Depths.Max(); }
        }
    }

    public class EnvironmentDataField<T> : SerializableData<EnvironmentDataField<T>> where T : EarthCoordinate
    {
        public EarthCoordinateList<T> Values { get; set; }
    }

    public class EarthCoordinateList<T> : List<T> where T: EarthCoordinate
    {
        #region Public properties

        /// <summary>
        ///   List of latitudes (in degrees) for which we have values
        /// </summary>
        public IEnumerable<double> Latitudes
        {
            get
            {
                var result = from value in this
                             select value.Latitude;
                result.Distinct().ToList().Sort();
                return result;
            }
        }

        /// <summary>
        ///   List of longitudes (in degrees) for which we have values
        /// </summary>
        public IEnumerable<double> Longitudes
        {
            get
            {
                var result = from value in this
                             select value.Longitude;
                result.Distinct().ToList().Sort();
                return result;
            }
        }

        /// <summary>
        ///   Corner of the data set that has the minimum lat and lon values
        /// </summary>
        public EarthCoordinate MinCoordinate { get { return new EarthCoordinate(Latitudes.First(), Longitudes.First()); } }

        /// <summary>
        ///   Corner of the data set that has the maximum lat and lon values
        /// </summary>
        public EarthCoordinate MaxCoordinate { get { return new EarthCoordinate(Latitudes.Last(), Longitudes.Last()); } }

        public double HorizontalResolution
        {
            get
            {
                var lonList = Longitudes.ToList();
                return Math.Abs(lonList[1] - lonList[0]);
            }
        }

        public double VerticalResolution
        {
            get
            {
                var latList = Longitudes.ToList();
                return Math.Abs(latList[1] - latList[0]);
            }
        }

        public OverlayLineSegments BoundingBox
        {
            get
            {
                var max = MaxCoordinate;
                var min = MinCoordinate;
                var vertRes = VerticalResolution;
                var horizRes = HorizontalResolution;
                var bathyBox = new[]
                               {
                                   //edit: Modified this routine to take the horizontal and vertical resolution into account
                                   //      It now places the bounding box such that the lines are coincident with the edges of
                                   //      the edge samples of the selected data (extends by half the horizontal/vertical resolution)
                                   //northeast corner:                   
                                   new EarthCoordinate(max.Latitude + (vertRes/2), max.Longitude + (horizRes/2)), //southeast corner: 
                                   new EarthCoordinate(min.Latitude - (vertRes/2), max.Longitude + (horizRes/2)), //southwest corner: 
                                   new EarthCoordinate(min.Latitude - (vertRes/2), min.Longitude - (horizRes/2)), //northwest corner: 
                                   new EarthCoordinate(max.Latitude + (vertRes/2), min.Longitude - (horizRes/2)), //northeast corner again to close the loop.
                                   new EarthCoordinate(max.Latitude + (vertRes/2), max.Longitude + (horizRes/2)),
                               };

                var shape = new OverlayLineSegments(bathyBox, Colors.Black, 1, LineStyle.Solid);
                return shape;
            }
        }

        #endregion

        protected EarthCoordinate ClosestPointTo(EarthCoordinate desiredLocation)
        {
            var distances = from value in this
                            select new
                            {
                                Value = value,
                                Distance = value.GetDistanceTo_Meters(desiredLocation)
                            };
            var result = from distance in distances
                         orderby distance.Distance
                         select distance;
            return result.First().Value;
        }

        public bool ContainsCoordinate(EarthCoordinate coordinate)
        {
            var min = MinCoordinate;
            var max = MaxCoordinate;
            return (min.Longitude <= coordinate.Longitude) && (coordinate.Longitude <= max.Longitude) && (min.Latitude <= coordinate.Latitude) && (coordinate.Latitude <= max.Latitude);
        }

        public EarthCoordinate[,] ValueArray
        {
            get
            {
                var lons = Longitudes.ToArray();
                var lats = Latitudes.ToArray();
                var result = new EarthCoordinate[lons.Length, lats.Length];
                for (var latIndex = 0; latIndex < lats.Length; latIndex++)
                    for (var lonIndex = 0; lonIndex < lons.Length; lonIndex++)
                    {
                        var lat = lats[latIndex];
                        var lon = lons[lonIndex];
                        var selectedPoint = Find(t => t.Latitude == lat && t.Longitude == lon);
                        if (selectedPoint == null) throw new DataException("ValueArray: This data set is not a rectangular array");
                        result[lonIndex, latIndex] = selectedPoint;
                    }
                return result;
            }
        }
    }

    public class NewEnvironment2DData : EnvironmentDataField<EnvironmentDataPoint<float>>
    {
        public EnvironmentDataPoint<float> ClosestPointTo(EarthCoordinate desiredLocation) { return (EnvironmentDataPoint<float>) Values.ClosestPointTo(desiredLocation); }

        public float[,] ValueArray
        {
            get 
            { 
                var lons = Values.Longitudes.ToArray();
                var lats = Values.Latitudes.ToArray();
                var result = new float[lons.Length,lats.Length];
                for (var latIndex = 0; latIndex < lats.Length; latIndex++)
                    for (var lonIndex = 0; lonIndex < lons.Length; lonIndex++)
                    {
                        var lat = lats[latIndex];
                        var lon = lons[lonIndex];
                        var selectedPoint = Values.Find(t => t.Latitude == lat && t.Longitude == lon);
                        if (selectedPoint == null) throw new DataException("ValueArray: This data set is not a rectangular array");
                        result[lonIndex, latIndex] = selectedPoint.Value;
                    }
                return result;
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
        protected virtual bool Lookup(EarthCoordinate coordinate, ref EnvironmentDataPoint<float> value)
        {
            var selectedIndex = Values.FindIndex(t => t.Latitude == coordinate.Latitude && t.Longitude == coordinate.Longitude);
            if (selectedIndex != -1)
            {
                value = Values[selectedIndex];
                return true;
            }
            return false;

        }
        public static NewEnvironment2DData ReadChrtrBinaryFile(string fileName)
        {
            var result = new NewEnvironment2DData();
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
                var maxDepth = -stream.ReadSingle();
                var minDepth = -stream.ReadSingle();
                var paddingWidth = (width - 10) * 4;
                stream.ReadBytes(paddingWidth);
                var depths = new float[height, width];
                for (var lat = 0; lat < height; lat++)
                    for (var lon = 0; lon < width; lon++)
                    {
                        var curSample = stream.ReadSingle();
                        result.Values.Add(new EnvironmentDataPoint<float>
                                          {
                                              Latitude = south + (lat * gridSpacing),
                                              Longitude = west + (lon * gridSpacing),
                                              Value = curSample == 1e16f ? float.NaN : -curSample,
                                          });
                    }
                return result;
            }
        }

    }

    public class Environment3DData : EnvironmentDataField<List<EnvironmentDataPoint<DepthValueList>>>
    {
        public new EnvironmentDataPoint<DepthValueList> ClosestPointTo(EarthCoordinate desiredLocation) { return (EnvironmentDataPoint<DepthValueList>)base.ClosestPointTo(desiredLocation); }

        public float[,] ValueArray(int depthIndex)
        {
            var lons = Longitudes.ToArray();
            var lats = Latitudes.ToArray();
            var result = new float[lons.Length,lats.Length];
            for (var latIndex = 0; latIndex < lats.Length; latIndex++)
                for (var lonIndex = 0; lonIndex < lons.Length; lonIndex++)
                {
                    var lat = lats[latIndex];
                    var lon = lons[lonIndex];
                    var selectedPoint = Values.Find(t => t.Latitude == lat && t.Longitude == lon);
                    if (selectedPoint == null) throw new DataException("ValueArray: This data set is not a rectangular array");
                    result[lonIndex, latIndex] = selectedPoint.Value.Values[depthIndex];
                }
            return result;
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
        protected virtual bool Lookup(EarthCoordinate coordinate, ref EnvironmentDataPoint<DepthValueList> value)
        {
            var selectedIndex = Values.FindIndex(t => t.Latitude == coordinate.Latitude && t.Longitude == coordinate.Longitude);
            if (selectedIndex != -1)
            {
                value = Values[selectedIndex];
                return true;
            }
            return false;
        }
    }
#endif

    public class SoundSpeedField : SerializableData<SoundSpeedField>
    {
        public string TimePeriod { get; set; }
        public List<SoundSpeedProfile> SoundSpeedProfiles { get; set; }

        [XmlIgnore]
        public SoundSpeedProfile DeepestSSP { get; private set; }

        public SoundSpeedField()
        {
            SoundSpeedProfiles = new List<SoundSpeedProfile>();
            foreach (var profile in SoundSpeedProfiles) DeepestSSP = (DeepestSSP != null) ? (DeepestSSP.MaxDepth < profile.MaxDepth ? profile : DeepestSSP) : profile;
        }

        public SoundSpeedField(SerializedOutput serializedOutput, string timePeriod)
        {
            TimePeriod = timePeriod;
            SoundSpeedProfiles = new List<SoundSpeedProfile>();
            foreach (var profile in serializedOutput.DataPoints)
                SoundSpeedProfiles.Add(new SoundSpeedProfile(profile, serializedOutput.DepthAxis));
            foreach (var profile in SoundSpeedProfiles) DeepestSSP = (DeepestSSP != null) ? (DeepestSSP.MaxDepth < profile.MaxDepth ? profile : DeepestSSP) : profile;
        }
        /// <summary>
        ///   List of latitudes (in degrees) for which we have values
        /// </summary>
        public IEnumerable<double> Latitudes
        {
            get
            {
                var latList = from value in SoundSpeedProfiles
                             select value.Latitude;
                var result = latList.Distinct().ToList();
                result.Sort();
                return result;
            }
        }

        /// <summary>
        ///   List of longitudes (in degrees) for which we have values
        /// </summary>
        public IEnumerable<double> Longitudes
        {
            get
            {
                var lonList = from value in SoundSpeedProfiles
                              select value.Longitude;
                var result = lonList.Distinct().ToList();
                result.Sort();
                return result;
            }
        }

        public IEnumerable<float> Depths
        {
            get
            {
                var depths = new List<float>();
                foreach (var profile in SoundSpeedProfiles)
                    depths.AddRange(profile.Depths);

                var result = depths.Distinct().ToList();
                result.Sort();
                return result;
            }
        }

        public static explicit operator SerializedOutput(SoundSpeedField ssf)
        {
            var result = new SerializedOutput();
            result.DepthAxis.AddRange(from depth in ssf.Depths
                                      select (double) depth);
            foreach (var profile in ssf.SoundSpeedProfiles)
                result.DataPoints.Add((EnvironmentalDataPoint)profile);
            return result;
        }

        public void ExtendProfilesToDepth(float maxDepth, SerializedOutput temperatureData, SerializedOutput salinityData)
        {
            if ((temperatureData == null) || (salinityData == null)) 
                throw new ApplicationException("SoundSpeedField: Unable to extend to max bathymetry depth.  Temperature and salinity data are missing.");

            if (maxDepth > DeepestSSP.MaxDepth)
            {

                var temps = new SoundSpeedField(temperatureData, TimePeriod);
                var sals = new SoundSpeedField(salinityData, TimePeriod);
                var deepestTemperature = temps[DeepestSSP];
                var deepestSalinity = sals[DeepestSSP];
                var tempD = deepestTemperature.SoundSpeeds.Last();
                var tempD1 = deepestTemperature.SoundSpeeds[DeepestSSP.SoundSpeeds.Length - 2];
                var salinity = deepestSalinity.SoundSpeeds.Last();

                var tempDiff = tempD1 - tempD;
                var newTemp = tempD - tempDiff;
                var soundSpeed = ChenMilleroLi.SoundSpeed(DeepestSSP, maxDepth, newTemp, salinity);
                DeepestSSP.Extend(maxDepth, soundSpeed);
            }
            foreach (var profile in SoundSpeedProfiles)
                if (profile != DeepestSSP)
                    profile.Extend(DeepestSSP);
        }

        public SoundSpeedField(string environmentFileName)
        {
            var file = DataFile.Open(environmentFileName);

            TimePeriod = null;
            SoundSpeedProfiles = null;
            var layer = file["soundspeed"];
            if (layer == null) throw new System.IO.FileFormatException(string.Format("SoundSpeedField: Specified environment file \"{0}\"does not contain a soundspeed layer", environmentFileName));
            TimePeriod = layer.TimePeriod;
            SoundSpeedProfiles = new List<SoundSpeedProfile>();
            SoundSpeedProfiles.AddRange(from row in layer.Rows
                                        from point in row.Points
                                        select new SoundSpeedProfile(point.EarthCoordinate, layer.DepthAxis.Values, point.Data));
            foreach (var profile in SoundSpeedProfiles) DeepestSSP = (DeepestSSP != null) ? (DeepestSSP.MaxDepth < profile.MaxDepth ? profile : DeepestSSP) : profile;
        }

        public SoundSpeedField(string environmentFileName, GeoRect extractionArea)
        {
            var file = DataFile.Open(environmentFileName);

            TimePeriod = null;
            SoundSpeedProfiles = null;
            var layer = file["soundspeed"];
            if (layer == null) throw new System.IO.FileFormatException(string.Format("SoundSpeedField: Specified environment file \"{0}\"does not contain a soundspeed layer", environmentFileName));
            TimePeriod = layer.TimePeriod;
            SoundSpeedProfiles = new List<SoundSpeedProfile>();
            SoundSpeedProfiles.AddRange(from row in layer.GetRows((float)extractionArea.South, (float)extractionArea.North)
                                        from point in row.GetPoints((float)extractionArea.West, (float)extractionArea.East)
                                        select new SoundSpeedProfile(point.EarthCoordinate, layer.DepthAxis.Values, point.Data));

            foreach (var profile in SoundSpeedProfiles) DeepestSSP = (DeepestSSP != null) ? (DeepestSSP.MaxDepth < profile.MaxDepth ? profile : DeepestSSP) : profile;
        }

        public SoundSpeedProfile ExtendSSP(SoundSpeedProfile shallowSSP, float requiredDepth)
        {
            var deltaV = float.NaN;
            int i;

            var extendedSSP = new SoundSpeedProfile
                              {
                                  Depths = DeepestSSP.Depths,
                                  SoundSpeeds = new float[DeepestSSP.Depths.Length]
                              };

            for (i = 0; i < DeepestSSP.Depths.Length; i++)
            {
                if (float.IsNaN(shallowSSP.SoundSpeeds[i])) break;
                deltaV = DeepestSSP.SoundSpeeds[i] - shallowSSP.SoundSpeeds[i];
                extendedSSP.SoundSpeeds[i] = shallowSSP.SoundSpeeds[i];
            }

            if (float.IsNaN(deltaV)) throw new ApplicationException("ExtendSSP: ShallowSSP is empty.");

            for (var j = i; j < DeepestSSP.Depths.Length; j++)
            {
                if (DeepestSSP.Depths[j - 1] <= requiredDepth) // need previous depth to be less than req'd, current is slightly greater
                    extendedSSP.SoundSpeeds[j] = DeepestSSP.SoundSpeeds[j] - deltaV;
                else extendedSSP.SoundSpeeds[j] = float.NaN;
            }
            return extendedSSP;
        }

        public SoundSpeedField(string timePeriod, List<SoundSpeedProfile> soundSpeedProfiles)
        {
            TimePeriod = timePeriod;
            SoundSpeedProfiles = soundSpeedProfiles;
        }

        public SoundSpeedProfile this[EarthCoordinate location]
        {
            get
            {
                var query = from p in SoundSpeedProfiles
                            orderby p.DistanceTo(location) ascending
                            select p;

                return query.Count() > 0 ? query.First() : SoundSpeedProfile.Empty;
            }
        }

        public void Save(string fileName)
        {
            Save(fileName, new[] { typeof(SoundSpeedProfile) });
        }

        public static SoundSpeedField Load(string fileName)
        {
            return Load(fileName, new[] { typeof(SoundSpeedProfile) });
        }

    }

    public class ReaderSSF
    {
        public static bool Validate(string ssfFilePath)
        {
            try
            {
                ReadFile(ssfFilePath, "");
                return true;
            }
            catch
            {
                return false;
            }
        }

        public static SoundSpeedField Read(string ssfPath, out string ssfFullPath)
        {
            if (Directory.Exists(ssfPath))
            {
                ssfFullPath = ssfPath;

                var pathArray = ssfPath.Split('\\');
                var date = pathArray[pathArray.Length - 1];

                var directoryInfo = new DirectoryInfo(ssfPath);
                var ssfFiles = directoryInfo.GetFiles("*_*x*.ssp");

                if (ssfFiles.Length == 0) throw new Exception("No Sound Speed Profiles found that match expected file naming convention.");

                var soundSpeedProfiles = new List<SoundSpeedProfile>();

                string fileName;
                string[] nameArray;
                foreach (var file in ssfFiles)
                {
                    fileName = file.Name.Substring(file.Name.LastIndexOf('_') + 1).Replace(".ssp", "");
                    nameArray = fileName.Split('x');

                    double latitude,
                           longitude;
                    if (file.Length > 0 && nameArray.Length == 2 && double.TryParse(nameArray[0], out latitude) && double.TryParse(nameArray[1], out longitude)) soundSpeedProfiles.Add(SoundSpeedProfile.Read(latitude, longitude, ssfPath));
                }

                if (soundSpeedProfiles.Count == 0) throw new Exception("No Sound Speed Profiles found that match expected file naming convention.");

                var soundSpeedFieldProperties = new SoundSpeedField(date, soundSpeedProfiles);

                ssfPath = ssfPath + ".txt";
                using (var streamWriter = new StreamWriter(ssfPath, false, Encoding.ASCII))
                {
                    foreach (var profile in soundSpeedFieldProperties.SoundSpeedProfiles)
                    {
                        streamWriter.WriteLine("Lat:  " + profile.Latitude.ToString("0.00##") + " Lon:  " + profile.Longitude.ToString("0.00##") + " Valid Days: 1-365");
                        streamWriter.WriteLine("Points in profile: " + profile.Depths.Length + " Min depth: " + profile.Depths[0].ToString("0.0") + " Max depth: " + profile.Depths[profile.Depths.Length - 1] + " Version: ESME 1.0 Distribution Statement A: Approved for public release. Distribution unlimited");

                        for (var i = 0; i < profile.Depths.Length; i++) streamWriter.WriteLine(profile.Depths[i].ToString("###0.0") + "\t00.000\t00.000\t" + profile.SoundSpeeds[i].ToString("###0.000"));
                    }

                    streamWriter.Flush();
                }

                return soundSpeedFieldProperties;
            }
            else
            {
                ssfFullPath = ssfPath = ssfPath + ".txt";

                if (!File.Exists(ssfPath)) throw new Exception("The specified Sound Speed Field file path does not exist.");

                var fileInfo = new FileInfo(ssfPath);
                var date = fileInfo.Name.Replace(fileInfo.Extension, "");

                if (fileInfo.Length == 0) throw new Exception("The specified Sound Speed Field file was empty.");

                var soundSpeedProfile = ReadFile(ssfPath, date);
                return new SoundSpeedField(date, soundSpeedProfile);
            }
        }

        static List<SoundSpeedProfile> ReadFile(string ssfPath, string date)
        {
            var profilesList = new List<SoundSpeedProfile>();

            using (var streamReader = new StreamReader(ssfPath, Encoding.ASCII))
            {
                string line;
                string[] lineArray;
                double latitude = -1,
                       longitude = -1;
                var points = 0;

                var depthList = new List<float>();
                var speedList = new List<float>();

                EarthCoordinate earthCoordinate = null;

                while ((line = streamReader.ReadLine()) != null)
                {
                    lineArray = line.Split(new[]
                                           {
                                               ' ', '\t'
                                           }, StringSplitOptions.RemoveEmptyEntries);
                    if (lineArray.Length >= 4 && lineArray[0] == "Lat:" && lineArray[2] == "Lon:" && double.TryParse(lineArray[1], out latitude) && double.TryParse(lineArray[3], out longitude)) {}
                    else if (lineArray.Length >= 9 && lineArray[0] == "Points" && int.TryParse(lineArray[3], out points) && points > 0)
                    {
                        earthCoordinate = new EarthCoordinate(latitude, longitude);
                    }
                    else
                    {
                        float depth;
                        float speed;
                        if (lineArray.Length == 4 && float.TryParse(lineArray[0], out depth) && float.TryParse(lineArray[3], out speed))
                        {
                            depthList.Add(depth);
                            speedList.Add(speed);
                        }
                    }

                    if (earthCoordinate != null && points > 0 && points == depthList.Count && points == speedList.Count)
                    {
                        var depthVector = depthList.ToArray();
                        var speedVector = speedList.ToArray();
                        profilesList.Add(new SoundSpeedProfile(earthCoordinate, depthVector, speedVector));

                        earthCoordinate = null;
                        points = 0;
                        depthList.Clear();
                        speedList.Clear();
                    }
                }

                if (profilesList.Count == 0) throw new Exception("There were no valid sound speed profiles found.");

                return profilesList;
            }
        }
    }
}
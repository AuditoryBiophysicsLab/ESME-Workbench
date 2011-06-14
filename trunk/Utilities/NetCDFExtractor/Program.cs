using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using ESME.Environment;
using ESME.Environment.NAVO;
using HRC.Navigation;
using NetCDF;

namespace ImportGDEM
{
    internal class Program
    {
        static void Main(string[] args)
        {
            string outputDirectory = null;
            var north = float.NaN;
            var south = float.NaN;
            var east = float.NaN;
            var west = float.NaN;
            string gdemRootDirectory = null;
            var temperatureFiles = new List<string>();
            var salinityFiles = new List<string>();
            var monthNames = new List<string>();

            if (args.Length < 1)
            {
                Usage();
                return;
            }
            for (var i = 0; i < args.Length; i++)
            {
                switch (args[i].ToLower())
                {
                    case "-out":
                    case "-output":
                        outputDirectory = args[++i];
                        break;
                    case "-gdem":
                        gdemRootDirectory = args[++i];
                        break;
                    case "-mons":
                    case "-months":
                        monthNames = GetStringListFromArg(args[++i], ',');
                        break;
                    case "-north":
                        north = float.Parse(args[++i]);
                        break;
                    case "-south":
                        south = float.Parse(args[++i]);
                        break;
                    case "-east":
                        east = float.Parse(args[++i]);
                        break;
                    case "-west":
                        west = float.Parse(args[++i]);
                        break;
                    default:
                        Usage();
                        return;
                }
            }

            if ((outputDirectory == null) || (!Directory.Exists(gdemRootDirectory)) || (monthNames.Count == 0) || (float.IsNaN(north) || float.IsNaN(south) || float.IsNaN(east) || float.IsNaN(west)))
            {
                Usage();
                return;
            }

            if ((north < -90) || (north > 90) || (south < -90) || (south > 90) || (north <= south) ||
                (west < -180) || (west > 180) || (east < -180) || (east > 180) || (east <= west))
            {
                Usage();
                return;
            }

            var months = monthNames.Select(curMonth => (NAVOTimePeriod)Enum.Parse(typeof (NAVOTimePeriod), curMonth)).ToList();
            months.Sort();
            foreach (var month in months)
            {
                temperatureFiles.Add(FindTemperatureFile(month, gdemRootDirectory));
                salinityFiles.Add(FindSalinityFile(month, gdemRootDirectory));
            }

            ImportGDEM(temperatureFiles, salinityFiles, months, outputDirectory, north, south, east, west);

            Console.WriteLine(@"success!");
        }

        static string FindSalinityFile(NAVOTimePeriod monthIndex, string gdemRootDirectory)
        {
            var files = Directory.GetFiles(gdemRootDirectory, GDEMSalinityFileName(monthIndex), SearchOption.AllDirectories);
            if (files.Length > 0) return files[0];
            files = Directory.GetFiles(gdemRootDirectory, NUWCSalinityFileName(monthIndex), SearchOption.AllDirectories);
            if (files.Length > 0) return files[0];
            throw new FileNotFoundException(string.Format("Could not find requested salinity file, tried {0} and {1}", GDEMSalinityFileName(monthIndex), NUWCSalinityFileName(monthIndex)));
        }

        static string FindTemperatureFile(NAVOTimePeriod monthIndex, string gdemRootDirectory)
        {
            var files = Directory.GetFiles(gdemRootDirectory, GDEMTemperatureFileName(monthIndex), SearchOption.AllDirectories);
            if (files.Length > 0) return files[0];
            files = Directory.GetFiles(gdemRootDirectory, NUWCTemperatureFileName(monthIndex), SearchOption.AllDirectories);
            if (files.Length > 0) return files[0];
            throw new FileNotFoundException(string.Format("Could not find requested temperature file, tried {0} and {1}", GDEMTemperatureFileName(monthIndex), NUWCTemperatureFileName(monthIndex)));
        }

        static string GDEMTemperatureFileName(NAVOTimePeriod monthIndex) { return "t" + BaseGDEMFileName(monthIndex); }
        static string GDEMSalinityFileName(NAVOTimePeriod monthIndex) { return "s" + BaseGDEMFileName(monthIndex); }
        static string BaseGDEMFileName(NAVOTimePeriod monthIndex) { return "gdemv3s" + string.Format("{0:00}", (int)monthIndex) + ".nc"; }
        static string NUWCTemperatureFileName(NAVOTimePeriod monthIndex) { return ShortMonthNames[(int)monthIndex] + "_t.nc"; }
        static string NUWCSalinityFileName(NAVOTimePeriod monthIndex) { return ShortMonthNames[(int)monthIndex] + "_s.nc"; }
        static readonly string[] ShortMonthNames = new[]
                                                  {
                                                      "noneuary", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"
                                                  };

        static List<string> GetStringListFromArg(string arg, char separator) { return arg.Split(separator).ToList(); }

        static void ImportGDEM(IList<string> temperatureFiles, IList<string> salinityFiles, IList<NAVOTimePeriod> months, string outputDirectory, float north, float south, float east, float west)
        { 
            var temperatureFile = new SoundSpeed();
            var salinityFile = new SoundSpeed();
            const string temperatureVariableName = "water_temp";
            const string salinityVariableName = "salinity";

            for (var monthIndex = 0; monthIndex < months.Count; monthIndex++)
            {
                Console.WriteLine("Importing temperature data for {0}...", months[monthIndex]);
                var temperatureField = ImportGDEM(temperatureFiles[monthIndex], temperatureVariableName, months[monthIndex], north, south, east, west);
                temperatureFile.SoundSpeedFields.Add(temperatureField);

                Console.WriteLine("Importing salinity data for {0}...", months[monthIndex]);
                var salinityField = ImportGDEM(salinityFiles[monthIndex], salinityVariableName, months[monthIndex], north, south, east, west);
                salinityFile.SoundSpeedFields.Add(salinityField);
            }

            Console.WriteLine("Saving imported temperature data...");
            temperatureFile.Save(Path.Combine(outputDirectory, "temperature.xml"));
            Console.WriteLine("Saving imported salinity data...");
            salinityFile.Save(Path.Combine(outputDirectory, "salinity.xml"));
        }

        static SoundSpeedField ImportGDEM(string fileName, string dataVarName, NAVOTimePeriod month, float north, float south, float east, float west)
        {
            const string lonVarName = "lon";
            const string latVarName = "lat";
            const string depthVarName = "depth";
            const string missingValueAttName = "missing_value";
            const string scaleFactorAttName = "scale_factor";
            const string offsetValueAttName = "add_offset";

            var myFile = new NcFile(fileName);
            int lonIndex, depthIndex;
            short missingValue;

            myFile.LoadAllData();

            var lonVar = myFile.Variables[lonVarName];
            var latVar = myFile.Variables[latVarName];
            var depthVar = myFile.Variables[depthVarName];
            var dataVar = myFile.Variables[dataVarName];
            
            var lons = new float[lonVar.ElementCount];
            for (var i = 0; i < lons.Length; i++) lons[i] = lonVar.GetFloat(i);
            var lats = new float[latVar.ElementCount];
            for (var i = 0; i < lats.Length; i++) lats[i] = latVar.GetFloat(i);
            
            if (lons.First() > west) west += 360;
            if (lons.Last() < west) west -= 360;
            if (lons.First() > east) east += 360;
            if (lons.Last() < east) east -= 360;

            var lonMap = new List<AxisMap>();
            var latMap = new List<AxisMap>();
            for (var i = 0; i < lons.Length; i++)
            {
                var temp = lonVar.GetFloat(i);
                if ((temp >= west) && (temp <= east)) lonMap.Add(new AxisMap(temp, i));
            }
            for (var i = 0; i < lats.Length; i++)
            {
                var temp = latVar.GetFloat(i);
                if (temp >= south && temp <= north) latMap.Add(new AxisMap(temp, i));
            }
            var selectedLons = lonMap.Select(x => x.Value).ToArray();
            var selectedLats = latMap.Select(x => x.Value).ToArray();

            var depthCount = (int) depthVar.Dimensions[0].Size;
            var depths = new float[depthCount];
            for (depthIndex = 0; depthIndex < depthCount; depthIndex++) depths[depthIndex] = depthVar.GetFloat(depthIndex);

            var latCount = selectedLats.Length;
            var lonCount = selectedLons.Length;

            var scaleFactor = 1.0f;
            var addOffset = 0.0f;
            if (!short.TryParse(missingValueAttName, out missingValue))
                if (missingValueAttName != String.Empty) missingValue = dataVar.Attributes[missingValueAttName].GetShort(0);

            if (scaleFactorAttName != String.Empty) scaleFactor = dataVar.Attributes[scaleFactorAttName].GetFloat(0);
            if (offsetValueAttName != String.Empty) addOffset = dataVar.Attributes[offsetValueAttName].GetFloat(0);

            var newField = new SoundSpeedField {TimePeriod = month};

            for (lonIndex = 0; lonIndex < lonCount; lonIndex++)
            {
                var lon = lonMap[lonIndex].Value;
                var wrappedLon = lon;
                while (wrappedLon > 180) wrappedLon -= 360;
                while (wrappedLon < -180) wrappedLon += 360;

                var lonSourceIndex = lonMap[lonIndex].Index;
                for (var latIndex = 0; latIndex < latCount; latIndex++)
                {
                    var lat = latMap[latIndex].Value;
                    var latSourceIndex = latMap[latIndex].Index;
                    var newProfile = new SoundSpeedProfile(new EarthCoordinate(lat, wrappedLon));
                    for (depthIndex = 0; depthIndex < depthCount; depthIndex++)
                    {
                        var curValue = dataVar.GetShort(depthIndex, latSourceIndex, lonSourceIndex);
                        if (curValue == missingValue) break;
                        newProfile.Data.Add(new DepthValuePair<float>(depths[depthIndex], ((curValue) * scaleFactor) + addOffset));
                    }
                    if (newProfile.Data.Count > 0) newField.EnvironmentData.Add(newProfile);
                }
            }
            return newField;
        }

        static void Usage()
        {
            Console.WriteLine(
                "Usage: ImportNetCDF -output <OutputDirectoryName>\n" +
                "                    -gdem <GDEMRootDirectory>\n" +
                "                    -months <MonthNameList>\n" +
                "                    -north <North>\n" +
                "                    -south <South> \n" +
                "                    -east <East> \n" +
                "                    -west <West> \n" +
                "\n" +
                "Where: <OutputDirectoryName> is the full path to a directory that will contain,\n" +
                "        the output files 'temperature.xml' and 'salinity.xml'.\n" +
                "\n" +
                "       <GDEMRootDirectory> is the root directory to search for the uncompressed\n" +
                "        GDEM netCDF source data. If this directory includes a space in it's name,\n" +
                "        be sure to wrap this entire argument in quotes.\n" +
                "\n" +
                "       <MonthNameList> is a comma-separated list of months to extract temperature\n" +
                "        and salinity data for.  Use complete month names, i.e. 'january' instead of\n" +
                "        'jan', and if you include spaces around commas, put this entire argument in\n" +
                "        quotes.\n" +
                "\n" +
                "       <North> is the northern boundary of the area to be extracted from the\n" +
                "        database.  Valid values are -90 to 90.  Must be greater than <South>.\n" +
                "\n" +
                "       <South> is the southern boundary of the area to be extracted from the\n" +
                "        database.  Valid values are -90 to 90. Must be less than <North>.\n" +
                "\n" +
                "       <East> is the eastern boundary of the area to be extracted from the\n" +
                "        database.  Valid values are -180 to 180.  Must be greater than <West>.\n" +
                "\n" +
                "       <West> is the western boundary of the area to be extracted from the\n" +
                "        database.  Valid values are -180 to 180.  Must be less than <East>.\n" +
                "\n");
        }
    }

    public class AxisMap : IComparable<AxisMap>, IEquatable<AxisMap>, IEquatable<float>, IEqualityComparer<AxisMap>
    {
        public float Value { get; set; }
        public int Index { get; set; }

        public AxisMap(float value, int index)
        {
            Value = value;
            Index = index;
        }

        public AxisMap(AxisMap toCopy)
        {
            Value = toCopy.Value;
            Index = toCopy.Index;
        }

        public AxisMap(BinaryReader stream)
        {
            Value = stream.ReadSingle();
            Index = stream.ReadInt32();
        }

        public void Save(BinaryWriter stream)
        {
            stream.Write(Value);
            stream.Write(Index);
        }

        public int CompareTo(AxisMap other)
        {
            return Value.CompareTo(other.Value);
        }

        public override string ToString()
        {
            return string.Format("({0}, {1})", Value, Index);
        }

        public bool Equals(AxisMap theOne, AxisMap theOther)
        {
            return theOne.Equals(theOther);
        }

        public int GetHashCode(AxisMap thing)
        {
            return thing.Value.GetHashCode();
        }

        public bool Equals(AxisMap other)
        {
            var product = Value / (double)(other.Value);
            return (0.99999 <= product) && (product < 1.00001);
        }

        public bool Equals(float other)
        {
            var product = Value / (double)other;
            return (0.99999 <= product) && (product < 1.00001);
        }
    }

}
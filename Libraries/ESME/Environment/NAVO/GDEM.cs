using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml.Serialization;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
    public class GDEM : NAVODataSource
    {
        public int MinMonth { get; set; }
        public int MaxMonth { get; set; }

        public override void ExtractArea(string filename, double north, double south, double east, double west)
        {
            var filepath = Path.GetDirectoryName(filename);

            //Select proper file(s) and set lon, lat, depth names, offset and scale factor values. 
            IEnumerable<string> ncFileList = Directory.EnumerateFiles(DatabasePath, "*.nc");
            var ncTemps = new List<string>();
            var ncSalts = new List<string>();

            foreach (string file in ncFileList)
            {
                string thisFile = Path.GetFileNameWithoutExtension(file);
                //i feel dirty now.
                if (thisFile.StartsWith("s"))
                {
                    int thisMonth = Int32.Parse(thisFile.TrimStart("sgdemv3s".ToCharArray()));
                    if (thisMonth >= MinMonth && thisMonth <= MaxMonth)
                    {
                        ncSalts.Add(file);
                    }
                }
                if (thisFile.StartsWith("t"))
                {
                    int thisMonth = Int32.Parse(thisFile.TrimStart("tgdemv3s".ToCharArray()));
                    if (thisMonth >= MinMonth && thisMonth <= MaxMonth)
                    {
                        ncTemps.Add(file);
                    }
                }
            }

            //extract temperature

            Environment3DData averageTemps = null;
            foreach (string file in ncTemps)
            {
                //extract temperature data into a XML file
                string temperatureString = Path.GetFileNameWithoutExtension(filename) + "-temperature.xml";
                string tempPath = Path.Combine(filepath, temperatureString);
                string tempOut = Path.GetTempFileName();
                
                CommandArgs = string.Format("-in {0} -lon lon -lat lat -north {1} -south {2} -east {3} -west {4} -dep depth -data {5} -sf {6} -offset {7} -out {8} -dataout {9} -force ", file, north, south, east, west, "water_temp", "scale_factor", "add_offset", Path.GetTempFileName(), tempPath);
                Execute();
                File.Delete(tempOut);
                //read it back in
                var serializer = new XmlSerializer(typeof (SerializedOutput));
                var reader = new StreamReader(tempPath);
                var temperatureOutput = (SerializedOutput) serializer.Deserialize(reader);

                //and add the temperature values to the list

                if (averageTemps == null) averageTemps = ExtractValues(temperatureOutput);
                else averageTemps.Add(ExtractValues(temperatureOutput));
            }
            if (averageTemps == null) throw new ApplicationException("no temperature data selected");
            if (ncTemps.Count > 1) averageTemps.DivideBy(ncTemps.Count);


            //extract salinity 
            Environment3DData averageSalinity = null;
            foreach (string file in ncSalts)
            {
                string salString = Path.GetFileNameWithoutExtension(filename) + "-salinity.xml";
                string salPath = Path.Combine(filepath, salString);
                string tempOut = Path.GetTempFileName();
                CommandArgs = string.Format("-in {0} -lon lon -lat lat -north {1} -south {2} -east {3} -west {4} -dep depth -data{5} -sf {6} -offset {7} -out {8} -dataout {9} -force ", file, north, south, east, west, "salinity", "scale_factor", "add_offset", Path.GetTempFileName(), salPath);
                Execute();
                File.Delete(tempOut);


                var serializer = new XmlSerializer(typeof (SerializedOutput));
                var reader = new StreamReader(salPath);
                var salinityOutput = (SerializedOutput) serializer.Deserialize(reader);

                if (averageSalinity == null) averageSalinity = ExtractValues(salinityOutput);
                else averageSalinity.Add(ExtractValues(salinityOutput));
            }
            if (averageSalinity == null) throw new ApplicationException("no salinity data selected"); //todo  
            if (ncTemps.Count > 1) averageSalinity.DivideBy(ncTemps.Count);

            //combine and get ssp.

            float[] depths = averageSalinity.Depths.Cast<float>().ToArray();
            var soundSpeeds = new Environment3DData(north, south, east, west, 1, depths, new List<float>[averageSalinity.Latitudes.Length,averageSalinity.Longitudes.Length]);
            for (int latIndex = 0; latIndex < averageSalinity.Latitudes.Length; latIndex++)
            {
                double lat = averageSalinity.Latitudes[latIndex];
                for (int lonIndex = 0; lonIndex < averageSalinity.Longitudes.Length; lonIndex++)
                {
                    double lon = averageSalinity.Longitudes[lonIndex];
                    var location = new EarthCoordinate(lat, lon);
                    float[] temps = averageTemps.Values[lonIndex, latIndex].ToArray();
                    float[] sals = averageSalinity.Values[lonIndex, latIndex].ToArray();
                    soundSpeeds.Values[lonIndex, latIndex] = UNESCO.SoundSpeed(location, ref depths, ref temps, ref sals).ToList();
                }
            }

            //tempvalues must contain ssp not temp or sal.
            ExtractedArea = soundSpeeds;
        }

        public override bool ValidateDataSource() { return false; }

        internal Environment3DData ExtractValues(SerializedOutput data)
        {
            var points = new Dictionary<string, List<float>>();
            var lats = new List<double>();
            var lons = new List<double>();
            List<float> depths = data.DepthAxis;

            foreach (EnvironmentalDataPoint point in data.DataPoints)
            {
                if (point.Data != null)
                {
                    points.Add(string.Format("{0:#.00000},{1:#.00000}", point.EarthCoordinate.Latitude_degrees, point.EarthCoordinate.Longitude_degrees), point.Data);
                    lats.Add(point.EarthCoordinate.Latitude_degrees);
                    lons.Add(point.EarthCoordinate.Longitude_degrees);
                }
            }
            List<double> uniqueLats = lats.Distinct().ToList();
            List<double> uniqueLons = lons.Distinct().ToList();
            uniqueLats.Sort();
            uniqueLons.Sort();

            var dataArray = new List<float>[uniqueLons.Count,uniqueLats.Count];
            for (int latIndex = 0; latIndex < uniqueLats.Count; latIndex++)
            {
                double lat = uniqueLats[latIndex];
                for (int lonIndex = 0; lonIndex < uniqueLons.Count; lonIndex++)
                {
                    double lon = uniqueLons[lonIndex];
                    string key = string.Format("{0:#.00000},{1:#.00000}", lat, lon);
                    dataArray[lonIndex, latIndex] = points[key];
                }
            }

            //populate tempvalues

            return new Environment3DData(uniqueLats.Last(), uniqueLats.First(), uniqueLons.Last(), uniqueLons.First(), 1, depths, dataArray);
        }
    }
}
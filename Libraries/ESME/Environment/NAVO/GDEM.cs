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
        public float GridSpacing { get; set; }

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

            Environment3DAverager averageTemps = null;
            foreach (string file in ncTemps)
            {
                //extract temperature data into a XML file
                string temperatureString = Path.GetFileNameWithoutExtension(file) + "-temperature.xml";
                string tempPath = Path.Combine(filepath, temperatureString);
                string tempOut = Path.GetTempFileName();

                CommandArgs = string.Format("-in \"{0}\" -lon lon -lat lat -north {1} -south {2} -east {3} -west {4} -dep depth -mv missing_value -data {5} -sf {6} -offset {7} -out \"{8}\" -dataout \"{9}\" -force ", file, north, south, east, west, "water_temp", "scale_factor", "add_offset", Path.GetTempFileName(), tempPath);
                Execute();
                File.Delete(tempOut);
                //read it back in
                var serializer = new XmlSerializer(typeof(SerializedOutput));
                var reader = new StreamReader(tempPath);
                var temperatureOutput = (SerializedOutput)serializer.Deserialize(reader);

                //and add the temperature values to the list

                var results = ExtractValues(temperatureOutput);
                var depthAxis = results.Depths.Select(x => (float)x).ToArray();
                var data = new List<AverageDatum>[results.Longitudes.Length, results.Latitudes.Length];
                if (averageTemps == null) averageTemps = new Environment3DAverager(results.Latitudes.Last(), results.Latitudes.First(), results.Longitudes.Last(), results.Longitudes.First(), GridSpacing, depthAxis, data);
                averageTemps.Add(results);
            }
            if (averageTemps == null) throw new ApplicationException("no temperature data selected");
            if (ncTemps.Count > 1) averageTemps.Average();

            //extract salinity 
            Environment3DAverager averageSalinity = null;
            foreach (string file in ncSalts)
            {
                string salString = Path.GetFileNameWithoutExtension(file) + "-salinity.xml";
                string salPath = Path.Combine(filepath, salString);
                string tempOut = Path.GetTempFileName();
                CommandArgs = string.Format("-in \"{0}\" -lon lon -lat lat -north {1} -south {2} -east {3} -west {4} -dep depth -mv missing_value -data {5} -sf {6} -offset {7} -out \"{8}\" -dataout \"{9}\" -force ", file, north, south, east, west, "salinity", "scale_factor", "add_offset", Path.GetTempFileName(), salPath);
                Execute();
                File.Delete(tempOut);


                var serializer = new XmlSerializer(typeof(SerializedOutput));
                var reader = new StreamReader(salPath);
                var salinityOutput = (SerializedOutput)serializer.Deserialize(reader);

                var results = ExtractValues(salinityOutput);
                var depthAxis = results.Depths.Select(x => (float)x).ToArray();
                var data = new List<AverageDatum>[results.Longitudes.Length, results.Latitudes.Length];
                if (averageSalinity == null) averageSalinity = new Environment3DAverager(results.Latitudes.Last(), results.Latitudes.First(), results.Longitudes.Last(), results.Longitudes.First(), GridSpacing, depthAxis, data);
                averageSalinity.Add(results);
            }
            if (averageSalinity == null) throw new ApplicationException("no salinity data selected"); //todo  
            if (ncTemps.Count > 1) averageSalinity.Average();

            //combine and get ssp.

            float[] depths = averageSalinity.Depths.Select(x => (float)x).ToArray();//Cast<float>().ToArray();
            var soundSpeeds = new Environment3DData(averageSalinity.Latitudes.Last(), averageSalinity.Latitudes.First(), averageSalinity.Longitudes.Last(), averageSalinity.Longitudes.First(), GridSpacing, depths, new List<float>[averageSalinity.Longitudes.Length, averageSalinity.Latitudes.Length]);
            for (int latIndex = 0; latIndex < averageSalinity.Latitudes.Length; latIndex++)
            {
                double lat = averageSalinity.Latitudes[latIndex];
                for (int lonIndex = 0; lonIndex < averageSalinity.Longitudes.Length; lonIndex++)
                {
                    double lon = averageSalinity.Longitudes[lonIndex];
                    var location = new EarthCoordinate(lat, lon);
                    if ((averageTemps.Values[lonIndex, latIndex] != null) && (averageSalinity.Values[lonIndex, latIndex] != null))
                    {
                        float[] temps = averageTemps.Values[lonIndex, latIndex].Select(x => x.Value).ToArray();
                        float[] sals = averageSalinity.Values[lonIndex, latIndex].Select(x => x.Value).ToArray();
                        soundSpeeds.Values[lonIndex, latIndex] = UNESCO.SoundSpeed(location, ref depths, ref temps, ref sals).ToList();
                    }
                }
            }
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

            var dataArray = new List<float>[uniqueLons.Count, uniqueLats.Count];
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



            return new Environment3DData(uniqueLats.Last(), uniqueLats.First(), uniqueLons.Last(), uniqueLons.First(), GridSpacing, depths, dataArray);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="ncFilenames"></param>
        /// <param name="dataType">The NetCDF parameter name of the data type to be extracted.  Commonly "salinity" or "water_temp". </param>
        /// <param name="outputPath">A path to the desired XML output directory.  Output files will be of the form outputPath/ncFilename-dataType.xml.  </param>
        /// <param name="north"></param>
        /// <param name="south"></param>
        /// <param name="east"></param>
        /// <param name="west"></param>
        /// <returns></returns>
        internal Environment3DAverager AverageGDEMOutput(List<string> ncFilenames, string dataType, string outputPath, double north, double south, double east, double west)
        {
            Environment3DAverager accumulator = null;
            foreach (var file in ncFilenames)
            {
                var dataString = Path.GetFileNameWithoutExtension(file) + dataType;
                var dataFilePath = Path.Combine(outputPath, dataString);
                var tempOut = Path.GetTempFileName(); //todo : remove entirely.

                //extract temperature data into a XML file
                //for sanity:
                const string lonParamName = "lon";
                const string latParamName = "lat";
                const string depthParamName = "depth";
                const string missingParamName = "missing_value";
                const string scaleParamName = "scale_factor";
                const string offsetParamName = "add_offset";
                CommandArgs = string.Format("-force -in \"{0}\" -lon {1} -lat {2} -north {3} -south {4} -east {5} -west {6} -dep {7}  -mv {8} -data {9} -sf {10} -offset {11} -out \"{12}\" -dataout \"{13}\"", file, latParamName,lonParamName, north, south, east, west, depthParamName, missingParamName, dataType, scaleParamName, offsetParamName, tempOut, dataFilePath);
                Execute();
                File.Delete(tempOut);

                //read it back in
                var serializer = new XmlSerializer(typeof(SerializedOutput));
                var reader = new StreamReader(dataFilePath);
                var dataOutput = (SerializedOutput)serializer.Deserialize(reader);

                //and add the values to the list

                var results = ExtractValues(dataOutput);
                var depthAxis = results.Depths.Select(x => (float)x).ToArray();
                var data = new List<AverageDatum>[results.Longitudes.Length, results.Latitudes.Length];
                if (accumulator == null) accumulator = new Environment3DAverager(results.Latitudes.Last(), results.Latitudes.First(), results.Longitudes.Last(), results.Longitudes.First(), GridSpacing, depthAxis, data);
                //and sum them.
                accumulator.Add(results);
            }
            if (accumulator == null) throw new ApplicationException(string.Format("no {0} data was extracted.", dataType));
            //then average them. 
            if (ncFilenames.Count > 1) accumulator.Average();

            return accumulator;
        }
    }
}
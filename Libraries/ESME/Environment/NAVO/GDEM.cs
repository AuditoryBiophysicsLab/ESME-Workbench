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
            //Determine which netCDF files we need to read.
            var ncFileList = Directory.EnumerateFiles(DatabasePath, "*.nc");
            var ncTemps = new List<string>();
            var ncSalts = new List<string>();

            foreach (var file in ncFileList)
            {
                var thisFile = Path.GetFileNameWithoutExtension(file);
                //i feel dirty now.
                if (thisFile.StartsWith("s"))
                {
                    var thisMonth = Int32.Parse(thisFile.TrimStart("sgdemv3s".ToCharArray()));
                    if (thisMonth >= MinMonth && thisMonth <= MaxMonth)
                    {
                        ncSalts.Add(file);
                    }
                }
                if (thisFile.StartsWith("t"))
                {
                    var thisMonth = Int32.Parse(thisFile.TrimStart("tgdemv3s".ToCharArray()));
                    if (thisMonth >= MinMonth && thisMonth <= MaxMonth)
                    {
                        ncTemps.Add(file);
                    }
                }
            }

            //extract average temperature and salinity from the right files for the season
            var filepath = Path.GetDirectoryName(filename);
            var averageTemps = AveragedGDEMOutput(ncTemps, "water_temp", filepath, north, south, east, west);
            var averageSalinity = AveragedGDEMOutput(ncSalts, "salinity", filepath, north, south, east, west);
            //sanity check: are averageTemps and averageSalinity in the same place?
            AreEqual(averageTemps.Depths, averageSalinity.Depths);
            AreEqual(averageTemps.Latitudes, averageSalinity.Latitudes);
            AreEqual(averageTemps.Longitudes, averageSalinity.Longitudes);
            //if so, it doesn't matter which one's GIS data we use. 
            var depths = averageSalinity.Depths.Select(x => (float)x).ToArray();
            var latitudes = averageSalinity.Latitudes;
            var longitudes = averageSalinity.Longitudes;
            
            //combine and get ssp.
            var soundSpeeds = new Environment3DData(latitudes.Last(), latitudes.First(), longitudes.Last(), longitudes.First(), GridSpacing, depths, new List<float>[longitudes.Length, latitudes.Length]);
            for (var latIndex = 0; latIndex < latitudes.Length; latIndex++)
            {
                var lat = latitudes[latIndex];
                for (var lonIndex = 0; lonIndex < longitudes.Length; lonIndex++)
                {
                    var lon = longitudes[lonIndex];
                    var location = new EarthCoordinate(lat, lon);
                    if ((averageTemps.Values[lonIndex, latIndex] != null) && (averageSalinity.Values[lonIndex, latIndex] != null))
                    {
                        var temps = averageTemps.Values[lonIndex, latIndex].Select(x => x.Value).ToArray();
                        var sals = averageSalinity.Values[lonIndex, latIndex].Select(x => x.Value).ToArray();
                        soundSpeeds.Values[lonIndex, latIndex] = UNESCO.SoundSpeed(location, ref depths, ref temps, ref sals).ToList();
                    }
                }
            }
            ExtractedArea = soundSpeeds;
        }

        public override bool ValidateDataSource() { return false; }

        internal void AreEqual(double[] arrayOne, double[] arrayTwo)
        {
            if(arrayOne.Where((value,i) => value != arrayTwo[i]).Any()) throw new ApplicationException("arrays are not equal.");
        }

        internal Environment3DData ExtractValues(SerializedOutput data)
        {
            var points = new Dictionary<string, List<float>>();
            var lats = new List<double>();
            var lons = new List<double>();
            var depths = data.DepthAxis;

            foreach (var point in data.DataPoints.Where(point => point.Data != null)) {
                points.Add(string.Format("{0:#.00000},{1:#.00000}", point.EarthCoordinate.Latitude_degrees, point.EarthCoordinate.Longitude_degrees), point.Data);
                lats.Add(point.EarthCoordinate.Latitude_degrees);
                lons.Add(point.EarthCoordinate.Longitude_degrees);
            }
            var uniqueLats = lats.Distinct().ToList();
            var uniqueLons = lons.Distinct().ToList();
            uniqueLats.Sort();
            uniqueLons.Sort();

            var dataArray = new List<float>[uniqueLons.Count, uniqueLats.Count];
            for (var latIndex = 0; latIndex < uniqueLats.Count; latIndex++)
            {
                var lat = uniqueLats[latIndex];
                for (var lonIndex = 0; lonIndex < uniqueLons.Count; lonIndex++)
                {
                    var lon = uniqueLons[lonIndex];
                    var key = string.Format("{0:#.00000},{1:#.00000}", lat, lon);
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
        internal Environment3DAverager AveragedGDEMOutput(List<string> ncFilenames, string dataType, string outputPath, double north, double south, double east, double west)
        {
            Environment3DAverager accumulator = null;
            foreach (var file in ncFilenames)
            {
                var dataString = Path.GetFileNameWithoutExtension(file) + "-" + dataType + ".xml" ;
                var dataFilePath = Path.Combine(outputPath, dataString);
                var tempOut = Path.GetTempFileName(); //todo : remove the need for this entirely.
                if(File.Exists(dataFilePath)) File.Delete(dataFilePath);

                //extract temperature data into a XML file
                //for sanity:
                const string lonParamName = "lon";
                const string latParamName = "lat";
                const string depthParamName = "depth";
                const string missingParamName = "missing_value";
                const string scaleParamName = "scale_factor";
                const string offsetParamName = "add_offset";
                CommandArgs = string.Format("-force -in \"{0}\" -lon {1} -lat {2} -north {3} -south {4} -east {5} -west {6} -dep {7}  -mv {8} -data {9} -sf {10} -offset {11} -out \"{12}\" -dataout \"{13}\"", file, lonParamName, latParamName, north, south, east, west, depthParamName, missingParamName, dataType, scaleParamName, offsetParamName, tempOut, dataFilePath);
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
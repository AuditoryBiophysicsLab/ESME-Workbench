using System.Collections.Generic;
using System.IO;
using System.Linq;
using Cinch;

namespace ESME.Environment.NAVO
{
    public static class SurfaceMarineGriddedClimatologyDatabase
    {
        static string _databasePath;
        public static string DatabasePath
        {
            get { return _databasePath; }
            set
            {
                if (_databasePath == value) return;
                _databasePath = value;
                if (!_databasePath.EndsWith(@"\")) _databasePath = _databasePath + @"\"; //database path has to end with a trailing slash here.  For SMGC, it's a directory, not a file.
            }
        }

        public static string ExtractionProgramPath { get; set; }

        public const float GridSpacing = 1.0f;

        public static string WindFilename(string outputPath, NAVOTimePeriod timePeriod) { return Path.Combine(outputPath, string.Format("{0}-wind.txt", timePeriod)); }

        //public static void ExtractArea(NAVOExtractionPacket extractionPacket)
        public static void ExtractArea(string outputDirectory, NAVOTimePeriod timePeriod, int startMonth, int endMonth, int monthsDuration, double north, double south, double east, double west)
        {
            var outputFilename = Path.Combine(outputDirectory, string.Format("{0}-wind.txt", timePeriod));

            var northPath = DatabasePath;
            var southPath = DatabasePath;
            if (Directory.Exists(Path.Combine(DatabasePath, "north"))) northPath = (Path.Combine(DatabasePath, @"north\"));
            if (Directory.Exists(Path.Combine(DatabasePath, "south"))) southPath = (Path.Combine(DatabasePath, @"south\"));

            System.Environment.SetEnvironmentVariable("SMGC_DATA_NORTH", northPath);
            System.Environment.SetEnvironmentVariable("SMGC_DATA_SOUTH", southPath);
            var commandArgs = string.Format("-lat {0}/{1} -lon {2}/{3} -mon {4}/{5} -par 17/1", south, north, west, east, startMonth, endMonth); // '-par 17/1' extracts wind speed statistical data.  don't ask. 
            var result = NAVOExtractionProgram.Execute(ExtractionProgramPath, commandArgs, outputDirectory);
            //result now contains the entire output of SMGC, i think, since it dumps data to STDOUT... so let's save it to disk in the right place. 
            using (var writer = new StreamWriter(outputFilename))
            {
                writer.WriteLine("StartMonth=" + startMonth);
                writer.WriteLine("EndMonth=" + endMonth);
                writer.WriteLine("MonthDuration=" + monthsDuration);
                writer.WriteLine("GridSpacing=" + GridSpacing);
                writer.Write(result);
            }
        }

        /// <summary>
        /// Parser for SMGC raw wind speed output. 
        /// </summary>
        /// <param name="fileName">The filename containing the SMGC output</param>
        /// <returns>a populated Environment2DData object with windspeeds per latitude/longitude.</returns>
        public static Environment2DData Parse(string fileName)
        {
            var resarray = File.ReadAllLines(fileName).ToList();
            var lats = new List<double>();
            var lons = new List<double>();
            //  var averagevalues = new List<double>();
            var rawvalues = new List<List<string>>();
            var points = new Dictionary<string, double>();
            //split the string up into lines
            var startMonth = int.Parse(resarray[0].Split('=')[1]);
            var endMonth = int.Parse(resarray[1].Split('=')[1]);
            var monthDuration = int.Parse(resarray[2].Split('=')[1]);
            var gridSpacing = int.Parse(resarray[3].Split('=')[1]);

            var curLineIndex = 0;
            var curGroupIndex = 0;
            while (curLineIndex < resarray.Count)
            {
                string thisline = resarray[curLineIndex++].Trim();
                if (curLineIndex >= resarray.Count) break;
                //if the line starts with 'Lat', add it plus everything up to the next blank line to rawvalues[i].
                if (thisline.StartsWith("Lat"))
                {
                    var curGroup = new List<string>
                                   {
                                       thisline.Trim()
                                   };
                    while (!string.IsNullOrEmpty(thisline = resarray[curLineIndex++]))
                    {
                        if (curLineIndex >= resarray.Count) break;
                        curGroup.Add(thisline.Trim());
                    }
                    rawvalues.Add(curGroup);
                    //if (curLineIndex >= resarray.Count) break;
                }
            }

            foreach (var curGroup in rawvalues)
            {
                var lat = double.NaN;
                var lon = double.NaN;
                var monthspeed = new List<double>();
                foreach (var dataline in from curLine in curGroup
                                         where !curLine.StartsWith("Lat") && !curLine.StartsWith("---") && !string.IsNullOrEmpty(curLine)
                                         select curLine.Split('\t'))
                {
                    if (double.TryParse(dataline[0], out lat) && double.TryParse(dataline[1], out lon)) monthspeed.Add(double.Parse(dataline[7]));
                    else throw new InvalidDataException("unexpected data in SMGC");
                }
                if (double.IsNaN(lat) || double.IsNaN(lon) || (monthspeed.Count <= 0)) continue;
                lats.Add(lat);
                lons.Add(lon);
                //averagevalues.Add(monthspeed.Average());
                points.Add(string.Format("{0:#.00000},{1:#.00000}", lat, lon), monthspeed.Average());
            }

            var uniqueLats = lats.Distinct().ToList();
            var uniqueLons = lons.Distinct().ToList();
            uniqueLats.Sort();
            uniqueLons.Sort();

            var dataArray = new float[uniqueLons.Count,uniqueLats.Count];
            for (var latIndex = 0; latIndex < uniqueLats.Count; latIndex++)
            {
                var lat = uniqueLats[latIndex];
                for (var lonIndex = 0; lonIndex < uniqueLons.Count; lonIndex++)
                {
                    var lon = uniqueLons[lonIndex];
                    var key = string.Format("{0:#.00000},{1:#.00000}", lat, lon);
                    double outval;
                    dataArray[lonIndex, latIndex] = points.TryGetValue(key, out outval) ? (float) outval : float.NaN;
                }
            }

            //and finally, make a useful thing out of them. 
            return new Environment2DData(uniqueLats.Last(), uniqueLats.First(), uniqueLons.Last(), uniqueLons.First(), gridSpacing, dataArray, 0, 0);
        }
    }
}
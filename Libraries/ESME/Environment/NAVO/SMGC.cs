using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace ESME.Environment.NAVO
{
    public class SMGC : NAVODataSource
    {
        public override void ExtractArea(NAVOExtractionPacket extractionPacket)
        {
            OutputFilename = Path.Combine(extractionPacket.Filename, string.Format("{0}-SMGC.txt", extractionPacket.TimePeriod));
            double north = extractionPacket.North;
            double south = extractionPacket.South;
            double east = extractionPacket.East;
            double west = extractionPacket.West;
            if (!DatabasePath.EndsWith("\\")) DatabasePath = DatabasePath + "\\"; //database path has to end with a trailing slash here.  For SMGC, it's a directory, not a file.

            string northPath = DatabasePath;
            string southPath = DatabasePath;
            if (Directory.Exists(Path.Combine(DatabasePath, "north"))) northPath = (Path.Combine(DatabasePath, @"north\"));
            if (Directory.Exists(Path.Combine(DatabasePath, "south"))) southPath = (Path.Combine(DatabasePath, @"south\"));

            System.Environment.SetEnvironmentVariable("SMGC_DATA_NORTH", northPath);
            System.Environment.SetEnvironmentVariable("SMGC_DATA_SOUTH", southPath);
            CommandArgs = string.Format("-lat {0}/{1} -lon {2}/{3} -mon {4}/{5} -par 17/1", south, north, west, east, StartMonth, EndMonth); // '-par 17/1' extracts wind speed statistical data.  don't ask. 
            string result = Execute();
            //result now contains the entire output of SMGC, i think, since it dumps data to STDOUT... so let's save it to disk in the right place. 
            using (var writer = new StreamWriter(OutputFilename))
            {
                writer.WriteLine("StartMonth=" + StartMonth);
                writer.WriteLine("EndMonth=" + EndMonth);
                writer.WriteLine("MonthDuration=" + MonthsDuration);
                writer.WriteLine("GridSpacing=" + GridSpacing);
                writer.Write(result);
            }
            //File.WriteAllText(filename, result);
            ExtractedArea = Parse(OutputFilename);
        }

        /// <summary>
        /// Parser for SMGC raw wind speed output. 
        /// </summary>
        /// <param name="fileName">The filename containing the SMGC output</param>
        /// <returns>a populated Environment2DData object with windspeeds per latitude/longitude.</returns>
        public static Environment2DData Parse(string fileName)
        {
            List<string> resarray = File.ReadAllLines(fileName).ToList();
            var lats = new List<double>();
            var lons = new List<double>();
            //  var averagevalues = new List<double>();
            var rawvalues = new List<List<string>>();
            var points = new Dictionary<string, double>();
            //split the string up into lines
            int startMonth = int.Parse(resarray[0].Split('=')[1]);
            int endMonth = int.Parse(resarray[1].Split('=')[1]);
            int monthDuration = int.Parse(resarray[2].Split('=')[1]);
            int gridSpacing = int.Parse(resarray[3].Split('=')[1]);

            int curLineIndex = 0;
            int curGroupIndex = 0;
            while (curLineIndex < resarray.Count)
            {
                string thisline = resarray[curLineIndex++].Trim();
                if (curLineIndex >= resarray.Count) break;
                //if the line starts with 'Lat', add it plus everything up to the next blank line to rawvalues[i].
                if (thisline.StartsWith("Lat"))
                {
                    var curGroup = new List<string>();
                    curGroup.Add(thisline.Trim());
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
                double lat = double.NaN;
                double lon = double.NaN;
                var monthspeed = new List<double>();
                foreach (string curLine in curGroup)
                {
                    if (!curLine.StartsWith("Lat") && !curLine.StartsWith("---") && !string.IsNullOrEmpty(curLine))
                    {
                        string[] dataline = curLine.Split('\t');

                        if (double.TryParse(dataline[0], out lat) && double.TryParse(dataline[1], out lon)) monthspeed.Add(double.Parse(dataline[7]));
                        else throw new InvalidDataException("unexpected data in SMGC");
                    }
                }
                if (!double.IsNaN(lat) && !double.IsNaN(lon) && (monthspeed.Count > 0))
                {
                    lats.Add(lat);
                    lons.Add(lon);
                    //averagevalues.Add(monthspeed.Average());
                    points.Add(string.Format("{0:#.00000},{1:#.00000}", lat, lon), monthspeed.Average());
                }
            }

            List<double> uniqueLats = lats.Distinct().ToList();
            List<double> uniqueLons = lons.Distinct().ToList();
            uniqueLats.Sort();
            uniqueLons.Sort();

            var dataArray = new float[uniqueLons.Count,uniqueLats.Count];
            for (int latIndex = 0; latIndex < uniqueLats.Count; latIndex++)
            {
                double lat = uniqueLats[latIndex];
                for (int lonIndex = 0; lonIndex < uniqueLons.Count; lonIndex++)
                {
                    double lon = uniqueLons[lonIndex];
                    string key = string.Format("{0:#.00000},{1:#.00000}", lat, lon);
                    double outval;
                    dataArray[lonIndex, latIndex] = points.TryGetValue(key, out outval) ? (float) outval : float.NaN;
                }
            }

            //and finally, make a useful thing out of them. 
            return new Environment2DData(uniqueLats.Last(), uniqueLats.First(), uniqueLons.Last(), uniqueLons.First(), gridSpacing, dataArray, 0, 0);
        }

        public override bool ValidateDataSource() { return false; } //SMGC provides no data validation routines 
    }
}
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace ESME.Environment.NAVO
{
    public class SMGC : NAVODataSource
    {
        public override void ExtractArea(NAVOExtractionPacket extractionPacket)
        {
            var filename = Path.Combine(Path.GetDirectoryName(extractionPacket.Filename), Path.GetFileNameWithoutExtension(extractionPacket.Filename) + "-SMGC.txt");
            var north = extractionPacket.North;
            var south = extractionPacket.South;
            var east = extractionPacket.East;
            var west = extractionPacket.West;
            if (!DatabasePath.EndsWith("\\")) DatabasePath = DatabasePath + "\\"; //database path has to end with a trailing slash here.  For SMGC, it's a directory, not a file.
            System.Environment.SetEnvironmentVariable("SMGC_DATA_NORTH", DatabasePath);
            System.Environment.SetEnvironmentVariable("SMGC_DATA_SOUTH", DatabasePath);
            CommandArgs = string.Format("-lat {0}/{1} -lon {2}/{3} -mon {4}/{5} -par 17/1", south, north, west, east, StartMonth, EndMonth); // '-par 17/1' extracts wind speed statistical data.  don't ask. 
            var result = Execute();
            //result now contains the entire output of SMGC, i think, since it dumps data to STDOUT... so let's save it to disk in the right place. 
            File.WriteAllText(filename, result);
            ExtractedArea = ParseSMGC(result);
        }

        /// <summary>
        /// Parser for SMGC raw wind speed output. 
        /// </summary>
        /// <param name="result">a string containing the total SMGC output</param>
        /// <returns>a populated Environment2DData object with windspeeds per latitude/longitude.</returns>
        Environment2DData ParseSMGC(string result)
        {
            var lats = new List<double>();
            var lons = new List<double>();
            var points = new Dictionary<string, double>();
            //split the string up into lines
            var resarray = result.Split('\n');
            for (var index = 0; index < resarray.Length; index++)
            {
                var line = resarray[index].Trim();
                //if the line has hyphens in it..
                if (line.Contains("---"))
                {
                    var rawspeeds = new double[(EndMonth - StartMonth) + 1];
                    var count = 0;
                    //then take the next lines of data and extract windspeed and positions from them.
                    for (var j = index + 1; j <= (index + (EndMonth - StartMonth) + 1); j++)
                    {
                        if (resarray[j].Equals("")) break;
                        var resline = resarray[j].Split('\t');
                        rawspeeds[count] = Double.Parse(resline[7]);
                        count++;
                    }
                    var latline = resarray[index + 1].Split('\t');
                    var lat = Double.Parse(latline[0]);
                    var lon = Double.Parse(latline[1]);
                    var meanspeed = rawspeeds.Sum()/rawspeeds.Length;
                    lats.Add(lat);
                    lons.Add(lon);
                    //then add these uniques to the dictionary
                    points.Add(string.Format("{0:#.00000},{1:#.00000}", lat, lon), meanspeed);
                }
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
                    dataArray[lonIndex, latIndex] = points.TryGetValue(key, out outval) ? (float)outval : float.NaN;
                }
            }
            
            //and finally, make a useful thing out of them. 
            return new Environment2DData(uniqueLats.Last(), uniqueLats.First(), uniqueLons.Last(), uniqueLons.First(), GridSpacing, dataArray, 0, 0);
        }

        public override bool ValidateDataSource() { return false; } //SMGC provides no data validation routines 
    }
}
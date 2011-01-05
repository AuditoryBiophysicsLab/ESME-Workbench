using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using ESME.Overlay;
using HRC.Navigation;
using FileFormatException = ESME.Model.FileFormatException;

namespace ESME.Environment.NAVO
{
    public class SMGC : NAVODataSource
    {
        public int MinMonth { get;  set; }
        public int MaxMonth { get;  set; }

        public SMGC()
        {

           
        }

        public override void ExtractArea(string filename, double north, double south, double east, double west)
        {
            if (!DatabasePath.EndsWith("\\")) DatabasePath = DatabasePath + "\\"; //database path has to end with a trailing slash here.  For SMGC, it's a directory, not a file.
            System.Environment.SetEnvironmentVariable("SMGC_DATA_NORTH", DatabasePath);
            System.Environment.SetEnvironmentVariable("SMGC_DATA_SOUTH", DatabasePath);
           var foo = System.Environment.GetEnvironmentVariable("SMGC_DATA_NORTH");
            CommandArgs = string.Format("-lat {0}/{1} -lon {2}/{3} -mon {4}/{5} -par 17/1", north, south, west, east, MinMonth, MaxMonth); // '-par 17/1' extracts wind speed statistical data.  don't ask. 
            var result = Execute();
            //result now contains the entire output of SMGC, i think, since it dumps data to STDOUT... so let's save it to disk in the right place. 
            File.WriteAllText(filename, result);
            ParseSMGC(result);

        }
        /// <summary>
        /// Parser for SMGC raw wind speed output. 
        /// </summary>
        /// <param name="result">a string containing the total SMGC output</param>
        /// <returns>a populated Environment2DData object with windspeeds per latitude/longitude.</returns>
        void ParseSMGC(string result)
        {
            var lats = new List<double>();
            var lons = new List<double>();
            var points = new Dictionary<string, double>();
            
            var resarray = result.Split('\n');
            for (int index = 0; index < resarray.Length; index++)
            {
                string line = resarray[index].Trim();
                if (line.Contains("---"))
                {
                    var rawspeeds = new double[(MaxMonth - MinMonth)+1];
                    var count = 0;
                    for (int j = index + 1; j <= (index + (MaxMonth-MinMonth)+1); j++)
                    {
                        if (resarray[j].Equals("")) 
                            break;
                        string[] resline = resarray[j].Split('\t');
                        rawspeeds[count] = Double.Parse(resline[7]);
                        count++;
                    }
                    string[] latline = resarray[index+1].Split('\t');
                    var lat = Double.Parse(latline[0]);
                    var lon = Double.Parse(latline[1]);
                    var meanspeed = rawspeeds.Sum() / rawspeeds.Length;
                    lats.Add(lat);
                    lons.Add(lon);
                    points.Add(string.Format("{0:#.00000},{1:#.00000}",lat, lon), meanspeed);
                }
            }
            var uniqueLats = lats.Distinct().ToList();
            var uniqueLons = lons.Distinct().ToList();
            uniqueLats.Sort();
            uniqueLons.Sort();
            var dataArray = new float[uniqueLons.Count,uniqueLats.Count];
            for (int latIndex = 0; latIndex < uniqueLats.Count; latIndex++)
            {
                var lat = uniqueLats[latIndex];
                for (int lonIndex = 0; lonIndex < uniqueLons.Count; lonIndex++)
                {
                    var lon = uniqueLons[lonIndex];
                    var key = string.Format("{0:#.00000},{1:#.00000}", lat, lon);
                    dataArray[lonIndex, latIndex] = (float)points[key];
                }
            }
            const float dataResolution = 1; //will change if SMGC ever gets better (but right now it's 1 degree resolution)
            ExtractedArea = new Environment2DData(uniqueLats.Last(), uniqueLats.First(), uniqueLons.Last(), uniqueLons.First(), dataResolution, dataArray, 0, 0);
        }

        public override bool ValidateDataSource() { return true; } //SMGC provides no data validation routines 
    }
   
}
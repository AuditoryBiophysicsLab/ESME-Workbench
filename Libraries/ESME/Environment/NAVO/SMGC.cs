using System;
using System.Collections.Generic;
using System.IO;
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
            CommandArgs = string.Format(" -lat {0}/{1} -lon {2}/{3} -mon {4}/{5} -par 17/1", south, north, west, east, MinMonth, MaxMonth); // '-par 17/1' extracts wind speed statistical data.  don't ask. 
            var result = Execute();
            //result now contains the entire output of SMGC, i think, since it dumps data to STDOUT... so let's save it to disk in the right place. 
            File.WriteAllText(filename, result);

        }

        Environment2DData ParseSMGC(string result)
        {
            var windspeed = new List<double>();
            var location = new List<EarthCoordinate>();
            var month = new List<string>();
            var resarray = result.Split('\n');
            for (int index = 0; index < resarray.Length; index++)
            {
                string line = resarray[index].Trim();
                if (line.Contains("---"))
                {
                    for (int j = index + 1; j < resarray.Length; j++)
                    {
                        if (resarray[j].Equals("")) 
                            break;
                        string[] resline = resarray[j].Split('\t');
                        month.Add(resline[2]);
                        location.Add(new EarthCoordinate(Double.Parse(resline[0]),Double.Parse(resline[1])));
                        windspeed.Add(Double.Parse(resline[7]));
                        

                        
                    }
                }
                
            }
            return new Environment2DData("")
                   {
                       Filename = "",
                   };
        }

        public override bool ValidateDataSource() { return true; } //SMGC provides no data validation routines 
    }
}
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace ESME.Environment.NAVO
{
    public class BST : NAVODataSource
    {
        public List<string> Resolutions { get; private set; }
        public string SelectedResolution { get; set; }

        public void GetAllResolutions()
        {
            Resolutions = new List<string>();
            const string contents = "set";
            var scriptfile = Path.GetTempFileName();
            File.WriteAllText(scriptfile, contents);

            CommandArgs = string.Format("\"{0}\" \"{1}\"", DatabasePath, scriptfile);

            var result = Execute().Trim();
            var resarray = result.Split('\n');
            if (!resarray[0].Contains("1.0")) throw new ApplicationException("BST database extraction utility version is not 1.0; parser should be checked/rewritten");
            for (var index = 0; index < resarray.Length; index++)
            {
                var line = resarray[index].Trim();
                if (line.Contains("Database Resolutions (minutes):"))
                {
                    for (var j = index + 1; j < resarray.Length; j++)
                    {
                        var resline = resarray[j].Trim();
                        if (resline.Equals("")) break;
                        Resolutions.Add(resline.Split('-')[1].Trim()); //?  dear LORD is this ugly and brittle.
                    }
                }
                if (line.Contains("could not be opened")) throw new ApplicationException("BST:  " + line);
            }

            File.Delete(scriptfile);
        }

        public override void ExtractArea(string filename, double north, double south, double east, double west)
        {
            var contents = string.Format("area {0} {1} {2} {3} {4} {5}.CHRTR", west, east, south, north, SelectedResolution, Path.Combine(Path.GetDirectoryName(filename), Path.GetFileNameWithoutExtension(filename)));
            var scriptfile = Path.GetTempFileName();
            File.WriteAllText(scriptfile, contents);

            CommandArgs = string.Format("\"{0}\" \"{1}\"", DatabasePath, scriptfile);
            var result = Execute();
            //have a look at result and throw an error if needed.
            var resarray = result.Split('\n');
            foreach (var line in resarray.Where(line => line.Contains("error"))) throw new ApplicationException("BST: " + line); //hope this is sufficient..
            ExtractedArea = Environment2DData.ReadChrtrBinaryFile(Path.Combine(Path.GetDirectoryName(filename), Path.GetFileNameWithoutExtension(filename)) + ".CHRTR");
            File.Delete(scriptfile);
        }

        public override bool ValidateDataSource()
        {
            //Test Case 1: Kattegat Area

            
            return false;
        }
    }
}
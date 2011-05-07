using System;
using System.Collections.Generic;
using System.IO;
using HRC.Navigation;

namespace ESME.TransmissionLoss.CASS
{
    public class SimAreaCSV : List<SimAreaDescriptor>
    {
        private SimAreaCSV() { }

        public static SimAreaCSV ReadCSV(string fileName)
        {
            var result = new SimAreaCSV
                         {
                             FileName = fileName
                         };
            var lines = File.ReadAllLines(fileName);
            var curLineNumber = 0;
            foreach (var line in lines)
            {
                curLineNumber++;
                if (line == null) continue;
                var curLine = line.Trim();
                if ((curLine.Trim() != "") && !curLine.StartsWith("!") && !curLine.StartsWith("#"))
                {
                    var fields = curLine.Split(',');
                    var simAreaName = fields[0];
                    var latString = fields[1];
                    var lonString = fields[2];
                    var heightString = fields[3];
                    var geoidString = fields[4];
                    var opsLimitFile = fields[5];
                    var simLimitFile = fields[6];
                    double latitude;
                    double longitude;
                    double height;
                    double geoid;
                    if (string.IsNullOrEmpty(simAreaName)) throw new FormatException(string.Format("SimAreaCSV: Error reading sim area file \"{0}\"\nLine number: {1}\nError: Invalid sim area name", fileName, curLineNumber));
                    if (string.IsNullOrEmpty(opsLimitFile)) throw new FormatException(string.Format("SimAreaCSV: Error reading sim area file \"{0}\"\nLine number: {1}\nError: Invalid OpsLimit filename", fileName, curLineNumber));
                    if (string.IsNullOrEmpty(simLimitFile)) throw new FormatException(string.Format("SimAreaCSV: Error reading sim area file \"{0}\"\nLine number: {1}\nError: Invalid SimLimit filename", fileName, curLineNumber));
                    if (!double.TryParse(latString, out latitude)) throw new FormatException(string.Format("SimAreaCSV: Error reading sim area file \"{0}\"\nLine number: {1}\nError: Invalid latitude", fileName, curLineNumber));
                    if (!double.TryParse(lonString, out longitude)) throw new FormatException(string.Format("SimAreaCSV: Error reading sim area file \"{0}\"\nLine number: {1}\nError: Invalid longitude", fileName, curLineNumber));
                    if (!double.TryParse(heightString, out height)) throw new FormatException(string.Format("SimAreaCSV: Error reading sim area file \"{0}\"\nLine number: {1}\nError: Invalid height", fileName, curLineNumber));
                    if (!double.TryParse(geoidString, out geoid)) throw new FormatException(string.Format("SimAreaCSV: Error reading sim area file \"{0}\"\nLine number: {1}\nError: Invalid geoid separation value", fileName, curLineNumber));
                    result.Add(new SimAreaDescriptor(latitude, longitude)
                               {
                                   Name = simAreaName,
                                   Height = height,
                                   GeoidSeparation = geoid,
                                   OpsLimitFile = opsLimitFile,
                                   SimLimitFile = simLimitFile,
                               });
                }
            }
            return result;
        }

        public SimAreaDescriptor this[string simAreaName]
        {
            get
            {
                foreach (var simArea in this)
                    if (simArea.Name.ToLower() == simAreaName.ToLower()) return simArea;
                throw new IndexOutOfRangeException(string.Format("SimAreaCSV: Requested sim area \"{0}\" not found in sim area file \"{1}\"", simAreaName, FileName));
            }
        }

        public string FileName { get; private set; }
    }

    public class SimAreaDescriptor : EarthCoordinate
    {
        internal SimAreaDescriptor(double latitude, double longitude) : base(latitude, longitude) {  }

        /// <summary>
        /// Name of the sim area
        /// </summary>
        public string Name { get; internal set; }

        /// <summary>
        /// Height of the sim area, in meters
        /// </summary>
        public double Height { get; internal set; }

        /// <summary>
        /// Geoid Separation, in meters
        /// </summary>
        public double GeoidSeparation { get; internal set; }

        /// <summary>
        /// OpsLimit filename
        /// </summary>
        public string OpsLimitFile { get; internal set; }

        /// <summary>
        /// SimLimit filename
        /// </summary>
        public string SimLimitFile { get; internal set; }
    }
}

﻿using System;
using System.IO;

namespace ESME.Environment.Descriptors
{
    public class RangeComplexDescriptors : NAEMODescriptors<RangeComplexDescriptor>
    {
        private RangeComplexDescriptors()
        {
        }

        public static RangeComplexDescriptors ReadCSV(string fileName)
        {
            var result = new RangeComplexDescriptors()
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
                    if (string.IsNullOrEmpty(simAreaName)) throw new FormatException(string.Format("RangeComplexDescriptors: Error reading sim area file \"{0}\"\nLine number: {1}\nError: Invalid sim area name", fileName, curLineNumber));
                    if (string.IsNullOrEmpty(opsLimitFile)) throw new FormatException(string.Format("RangeComplexDescriptors: Error reading sim area file \"{0}\"\nLine number: {1}\nError: Invalid OpsLimit filename", fileName, curLineNumber));
                    if (string.IsNullOrEmpty(simLimitFile)) throw new FormatException(string.Format("RangeComplexDescriptors: Error reading sim area file \"{0}\"\nLine number: {1}\nError: Invalid SimLimit filename", fileName, curLineNumber));
                    if (!double.TryParse(latString, out latitude)) throw new FormatException(string.Format("RangeComplexDescriptors: Error reading sim area file \"{0}\"\nLine number: {1}\nError: Invalid latitude", fileName, curLineNumber));
                    if (!double.TryParse(lonString, out longitude)) throw new FormatException(string.Format("RangeComplexDescriptors: Error reading sim area file \"{0}\"\nLine number: {1}\nError: Invalid longitude", fileName, curLineNumber));
                    if (!double.TryParse(heightString, out height)) throw new FormatException(string.Format("RangeComplexDescriptors: Error reading sim area file \"{0}\"\nLine number: {1}\nError: Invalid height", fileName, curLineNumber));
                    if (!double.TryParse(geoidString, out geoid)) throw new FormatException(string.Format("RangeComplexDescriptors: Error reading sim area file \"{0}\"\nLine number: {1}\nError: Invalid geoid separation value", fileName, curLineNumber));
                    result.Add(new System.Collections.Generic.KeyValuePair<string, RangeComplexDescriptor>(simAreaName, new RangeComplexDescriptor
                               {
                                   Data = new RangeComplex(latitude, longitude)
                                   {
                                       Name = simAreaName,
                                       Height = height,
                                       GeoidSeparation = geoid,
                                       OpsLimitFile = opsLimitFile,
                                       SimLimitFile = simLimitFile,
                                   }
                               }));
                }
            }
            return result;
        }

        public string FileName { get; private set; }
    }
}
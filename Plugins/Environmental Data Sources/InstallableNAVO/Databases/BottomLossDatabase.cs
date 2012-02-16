using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using ESME;
using ESME.Environment;
using ESME.NEMO;
using HRC.Navigation;

namespace NAVO.Databases
{
    public static class BottomLossDatabase
    {
        public static BottomLoss Extract(GeoRect region, IProgress<string> currentState = null, IProgress<float> progress = null)
        {
            var result = ExtractAsync(region, currentState, progress);
            return result.Result;
        }

        public async static Task<BottomLoss> ExtractAsync(GeoRect region, IProgress<string> currentState = null, IProgress<float> progress = null)
        {
            if (progress != null) lock (progress) progress.Report(0f);
            if (currentState != null) lock (currentState) currentState.Report("Importing bottom loss data");

            var locations = new List<EarthCoordinate>();
            var north = (float)Math.Ceiling(region.North);
            var south = (float)Math.Floor(region.South);
            var east = (float)Math.Ceiling(region.East);
            var west = (float)Math.Floor(region.West);

            for (var lat = south; lat < north; lat += 0.25f)
                for (var lon = west; lon < east; lon += 0.25f)
                    locations.Add(new EarthCoordinate(lat, lon));
            var progressStep = 100f / ((locations.Count * 2) + 1);
            var totalProgress = 0f;
            var useLFBL = !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.LFBLEXEPath);
            var useHFBL = !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.HFBLEXEPath);
            var totalPointsRequested = 0;
            var totalPointsProcessed = 0;
            var transformBlock = new TransformBlock<EarthCoordinate, BottomLossSample>(async location =>
                {
                    BottomLossSample curPoint = null;
                    if (useLFBL)
                    {
                        var process = Process.Start(new ProcessStartInfo
                        {
                            Arguments = ExtractorArgument(true, location),
                            FileName = Globals.AppSettings.NAVOConfiguration.LFBLEXEPath,
                            RedirectStandardInput = false,
                            RedirectStandardOutput = true,
                            RedirectStandardError = true,
                            UseShellExecute = false,
                            CreateNoWindow = true,
                            WorkingDirectory = Path.GetDirectoryName(Globals.AppSettings.NAVOConfiguration.LFBLEXEPath),
                        });
                        //process.PriorityClass = ProcessPriorityClass.Normal;
                        while (!process.HasExited) await TaskEx.Delay(50);
                        if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
                        //var stderr = process.StandardError.ReadToEnd();
                        curPoint = ParseLowFrequencyOutput(process.StandardOutput);
                        //Debug.WriteLine(string.Format("Low frequency output: {0}", process.StandardOutput.ReadToEnd()));
                    }
                    if (useHFBL)
                    {
                        var process = Process.Start(new ProcessStartInfo
                        {
                            Arguments = ExtractorArgument(false, location),
                            FileName = Globals.AppSettings.NAVOConfiguration.HFBLEXEPath,
                            RedirectStandardInput = false,
                            RedirectStandardOutput = true,
                            RedirectStandardError = true,
                            UseShellExecute = false,
                            CreateNoWindow = true,
                            WorkingDirectory = Path.GetDirectoryName(Globals.AppSettings.NAVOConfiguration.HFBLEXEPath),
                        });
                        //process.PriorityClass = ProcessPriorityClass.Normal;
                        while (!process.HasExited) await TaskEx.Delay(50);
                        if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
                        //var stderr = process.StandardError.ReadToEnd();
                        curPoint = ParseHighFrequencyOutput(process.StandardOutput, curPoint);
                        //Debug.WriteLine(string.Format("High frequency output: {0}", process.StandardOutput.ReadToEnd()));
                    }
                    Interlocked.Increment(ref totalPointsProcessed);
                    if (totalPointsProcessed % 1000 == 0) Debug.WriteLine("{0} BottomLossDatabase: {1:0.0}% complete", DateTime.Now, ((float)totalPointsProcessed / (float) totalPointsRequested) * 100);
                    return curPoint;
                },
                new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    MaxDegreeOfParallelism = 4,
                });

            var bottomLoss = new BottomLoss();
            foreach (var location in locations)
            {
                transformBlock.Post(location);
                totalPointsRequested++;
            }

            var batchBlock = new BatchBlock<BottomLossSample>(locations.Count);
            transformBlock.LinkTo(batchBlock);

            transformBlock.Complete();
            await transformBlock.Completion;
            bottomLoss.Samples.AddRange(batchBlock.Receive().Where(data => data != null).ToList());
            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            return bottomLoss;
        }

        static string ExtractorArgument(bool isLowFrequency, Geo location)
        {
            var database = Path.Combine(isLowFrequency ? Path.GetDirectoryName(Globals.AppSettings.NAVOConfiguration.LFBLEXEPath) : Path.GetDirectoryName(Globals.AppSettings.NAVOConfiguration.HFBLEXEPath), "dbases/");
            return string.Format(isLowFrequency ? "\"/\" \"{0}\" {1:0.00} {2:0.00} 1 0" : "\"/\" \"{0}\" {1:0.00} {2:0.00}", database, location.Latitude, location.Longitude);
        }

        static BottomLossSample ParseLowFrequencyOutput(TextReader stream)
        {
            var splitCharsSpaceEquals = new[] { ' ', '=' };
            //NextLine(stream);
            var fields = NextLine(stream).Split(splitCharsSpaceEquals, StringSplitOptions.RemoveEmptyEntries);
            if (fields[0].ToLower() != "latitude")
                throw new FormatException("Error parsing bottom loss results.  LFBL output not in expected format (latitude)");
            var lfLatitude = Math.Round(double.Parse(fields[1]), 4);
            fields = NextLine(stream).Split(splitCharsSpaceEquals, StringSplitOptions.RemoveEmptyEntries);
            if (fields[0].ToLower() != "longitude")
                throw new FormatException("Error parsing bottom loss results.  LFBL output not in expected format (longitude)");
            var lfLongitude = Math.Round(double.Parse(fields[1]), 4);
            var curPoint = new BottomLossSample(lfLatitude, lfLongitude, new BottomLossData());
            NextLine(stream, "---- World 15 Parameter set ----");
            var curLine = NextLine(stream);
            while (!curLine.Contains("Tabular Listing of Parameters"))
            {
                fields = curLine.Split(splitCharsSpaceEquals, StringSplitOptions.RemoveEmptyEntries);
                switch (fields[0].ToUpper())
                {
                    case "RATIOD":
                        curPoint.Data.RATIOD = double.Parse(fields[1]);
                        break;
                    case "DLD":
                        curPoint.Data.DLD = double.Parse(fields[1]);
                        break;
                    case "RHOLD":
                        curPoint.Data.RHOLD = double.Parse(fields[1]);
                        break;
                    case "RHOSD":
                        curPoint.Data.RHOSD = double.Parse(fields[1]);
                        break;
                    case "GD":
                        curPoint.Data.GD = double.Parse(fields[1]);
                        break;
                    case "BETAD":
                        curPoint.Data.BETAD = double.Parse(fields[1]);
                        break;
                    case "FKZD":
                        curPoint.Data.FKZD = double.Parse(fields[1]);
                        break;
                    case "FKZP":
                        curPoint.Data.FKZP = double.Parse(fields[1]);
                        break;
                    case "BRFLD":
                        curPoint.Data.BRFLD = double.Parse(fields[1]);
                        break;
                    case "FEXP":
                        curPoint.Data.FEXP = double.Parse(fields[1]);
                        break;
                    case "D2A":
                        curPoint.Data.D2A = double.Parse(fields[1]);
                        break;
                    case "ALF2A":
                        curPoint.Data.ALF2A = double.Parse(fields[1]);
                        break;
                    case "RHO2A":
                        curPoint.Data.RHO2A = double.Parse(fields[1]);
                        break;
                    case "SUBCRIT":
                        curPoint.Data.SUBCRIT = double.Parse(fields[1]);
                        break;
                    case "T2RH":
                        curPoint.Data.T2RH = double.Parse(fields[1]);
                        break;
                    case "SEDTHK_M":
                        curPoint.Data.SEDTHK_M = double.Parse(fields[1]);
                        break;
                    case "SEDTHK_S":
                        curPoint.Data.SEDTHK_S = double.Parse(fields[1]);
                        break;
                }
                curLine = NextLine(stream);
            }
            return curPoint;
        }

        static BottomLossSample ParseHighFrequencyOutput(TextReader stream, BottomLossSample curPoint)
        {
            var splitCharsSpaceEquals = new[] { ' ', '=' };
            var splitCharsCommaEquals = new[] { ',', '=' };

            var fields = NextLine(stream).Split(splitCharsSpaceEquals, StringSplitOptions.RemoveEmptyEntries);
            if (fields[0].ToLower() != "lat") throw new FormatException("Error parsing bottom loss results.  HFBL output not in expected format (lat)");
            var hfLatitude = Math.Round(double.Parse(fields[1]), 4);
            if (fields[2].ToLower() != "lon") throw new FormatException("Error parsing bottom loss results.  HFBL output not in expected format (lon)");
            var hfLongitude = Math.Round(double.Parse(fields[3]), 4);
            if (curPoint == null) curPoint = new BottomLossSample(hfLatitude, hfLongitude, new BottomLossData());
            else if (((int)Math.Round(hfLatitude * 10000.0) != (int)Math.Round(curPoint.Latitude * 10000)) && ((int)Math.Round(hfLongitude * 10000.0) != (int)Math.Round(curPoint.Longitude * 10000))) throw new FormatException("Error parsing bottom loss results.  Adjacent LFBL and HFBL extractions do not refer to the same point");
            fields = NextLine(stream).Split(splitCharsCommaEquals, StringSplitOptions.RemoveEmptyEntries);
            if (fields[0].Trim().ToUpper() != "HFBL CURVE NUMBER") throw new FormatException("Error parsing bottom loss results.  HFBL output not in expected format (HFBL curve number)");
            curPoint.Data.CurveNumber = double.Parse(fields[1]);
            return curPoint;
        }

        static string NextLine(TextReader stream, string lineContains = null, string currentLine = null)
        {
            if ((lineContains != null) && (string.IsNullOrEmpty(lineContains))) throw new ParameterOutOfRangeException("NextLine: lineContains cannot be String.Empty");
            
            if ((currentLine != null) && (lineContains != null) && (currentLine.Contains(lineContains))) return currentLine;

            var result = stream.ReadLine();
            while (result != null)
            {
                if (!string.IsNullOrEmpty(lineContains))
                    while (!result.Contains(lineContains))
                    {
                        result = stream.ReadLine();
                        if (result == null) return null;
                    }
                result = result.Trim();
                if (result != string.Empty) return result;

                result = stream.ReadLine();
            }
            return null;
        }
    }
}
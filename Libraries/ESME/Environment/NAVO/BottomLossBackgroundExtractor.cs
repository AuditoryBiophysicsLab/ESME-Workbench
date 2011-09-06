using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.Serialization.Formatters.Binary;
using Cinch;
using ESME.NEMO;
using GisSharpBlog.NetTopologySuite.IO;
using HRC.Navigation;
using System.Threading.Tasks;
using HRC.Utility;

namespace ESME.Environment.NAVO
{
    public class BottomLossBackgroundExtractor : NAVOBackgroundExtractor
    {
        #region public bool UseHFBL { get; set; }

        public bool UseHFBL
        {
            get { return _useHFBL; }
            set
            {
                if (_useHFBL == value) return;
                _useHFBL = value;
                NotifyPropertyChanged(UseHFBLChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs UseHFBLChangedEventArgs = ObservableHelper.CreateArgs<BottomLossBackgroundExtractor>(x => x.UseHFBL);
        bool _useHFBL;

        #endregion

        #region public bool UseLFBL { get; set; }

        public bool UseLFBL
        {
            get { return _useLFBL; }
            set
            {
                if (_useLFBL == value) return;
                _useLFBL = value;
                NotifyPropertyChanged(UseLFBLChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs UseLFBLChangedEventArgs = ObservableHelper.CreateArgs<BottomLossBackgroundExtractor>(x => x.UseLFBL);
        bool _useLFBL;

        #endregion

        #region public EnvironmentData<BottomLossData> BottomLossData { get; set; }

        public EnvironmentData<BottomLossSample> BottomLossData
        {
            get { return _bottomLossData; }
        }

        readonly EnvironmentData<BottomLossSample> _bottomLossData = new EnvironmentData<BottomLossSample>();

        #endregion

        #region public string ErrorText { get; set; }

        public string ErrorText
        {
            get { return _errorText; }
            set
            {
                if (_errorText == value) return;
                _errorText = value;
                NotifyPropertyChanged(ErrorTextChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ErrorTextChangedEventArgs = ObservableHelper.CreateArgs<BottomLossBackgroundExtractor>(x => x.ErrorText);
        string _errorText;

        #endregion

        #region public bool PointExtractionMode { get; set; }

        public bool PointExtractionMode
        {
            get { return _pointExtractionMode; }
            set
            {
                if (_pointExtractionMode == value) return;
                _pointExtractionMode = value;
                NotifyPropertyChanged(PointExtractionModeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PointExtractionModeChangedEventArgs = ObservableHelper.CreateArgs<BottomLossBackgroundExtractor>(x => x.PointExtractionMode);
        bool _pointExtractionMode;

        #endregion

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            RunState = "Running";
            TaskName = "Bottom Loss data extraction";
            var backgroundExtractor = (NAVOBackgroundExtractor)e.Argument;
            backgroundExtractor.Status = "Extracting bottom loss data";

            float north, south, east, west;
            var locations = new List<EarthCoordinate>();
            if (!PointExtractionMode)
            {
                north = (float)Math.Ceiling(backgroundExtractor.ExtractionArea.North);
                south = (float)Math.Floor(backgroundExtractor.ExtractionArea.South);
                east = (float)Math.Ceiling(backgroundExtractor.ExtractionArea.East);
                west = (float)Math.Floor(backgroundExtractor.ExtractionArea.West);
                locations = new List<EarthCoordinate>();
                for (var lat = south; lat < north; lat += 0.25f)
                    for (var lon = west; lon < east; lon += 0.25f)
                        locations.Add(new EarthCoordinate(lat, lon));
            }
            else
            {
                north = (float)backgroundExtractor.ExtractionArea.North;
                south = (float)backgroundExtractor.ExtractionArea.South;
                east = (float)backgroundExtractor.ExtractionArea.East;
                west = (float)backgroundExtractor.ExtractionArea.West;
                locations.Add(new EarthCoordinate(north, west));
            }

            backgroundExtractor.Maximum = locations.Count;
            foreach (var location in locations) 
            {
                BottomLossSample curPoint = null;
                if (UseLFBL)
                {
                    var process = Process.Start(new ProcessStartInfo
                    {
                        Arguments = ExtractorArgument(true, location),
                        FileName = NAVOConfiguration.LFBLEXEPath,
                        RedirectStandardInput = false,
                        RedirectStandardOutput = true,
                        RedirectStandardError = true,
                        UseShellExecute = false,
                        CreateNoWindow = true,
                    });
                    //process.PriorityClass = ProcessPriorityClass.Normal;
                    process.WaitForExit();
                    var stderr = process.StandardError.ReadToEnd();
                    curPoint = ParseLowFrequencyOutput(process.StandardOutput);
                }
                if (UseHFBL)
                {
                    var process = Process.Start(new ProcessStartInfo
                    {
                        Arguments = ExtractorArgument(false, location),
                        FileName = NAVOConfiguration.HFBLEXEPath,
                        RedirectStandardInput = false,
                        RedirectStandardOutput = true,
                        RedirectStandardError = true,
                        UseShellExecute = false,
                        CreateNoWindow = true,
                    });
                    //process.PriorityClass = ProcessPriorityClass.Normal;
                    process.WaitForExit();
                    var stderr = process.StandardError.ReadToEnd();
                    curPoint = ParseHighFrequencyOutput(process.StandardOutput, curPoint);
                }
                if (curPoint != null) BottomLossData.Add(curPoint);
                backgroundExtractor.Value++;
            }
        }

        public static void Extract(bool useHFBL, bool useLFBL, bool isPointExtraction, float north, float south, float east, float west, string outputFilename, IProgress<TaskProgressInfo> progress = null)
        {
            var taskProgress = new TaskProgressInfo
            {
                TaskName = "Bottom Loss Extraction",
                CurrentActivity = "Initializing",
                ProgressPercent = 0,
            };
            if (progress != null) progress.Report(taskProgress);

            var locations = new List<EarthCoordinate>();
            if (!isPointExtraction)
            {
                north = (float)Math.Ceiling(north);
                south = (float)Math.Floor(south);
                east = (float)Math.Ceiling(east);
                west = (float)Math.Floor(west);
                locations = new List<EarthCoordinate>();
                for (var lat = south; lat < north; lat += 0.25f)
                    for (var lon = west; lon < east; lon += 0.25f)
                        locations.Add(new EarthCoordinate(lat, lon));
            }
            else
            {
                locations.Add(new EarthCoordinate(north, west));
            }

            var bottomLoss = new BottomLoss();
            foreach (var location in locations)
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
                    process.WaitForExit();
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
                    process.WaitForExit();
                    //var stderr = process.StandardError.ReadToEnd();
                    curPoint = ParseHighFrequencyOutput(process.StandardOutput, curPoint);
                    //Debug.WriteLine(string.Format("High frequency output: {0}", process.StandardOutput.ReadToEnd()));
                }
                if (curPoint != null) bottomLoss.Samples.Add(curPoint);
            }
            taskProgress.CurrentActivity = "Saving";
            taskProgress.ProgressPercent = 95;
            if (progress != null) progress.Report(taskProgress);
            bottomLoss.Save(outputFilename + ".bottomloss");
            taskProgress.CurrentActivity = "Done";
            taskProgress.ProgressPercent = 100;
            if (progress != null) progress.Report(taskProgress);

        }

        void GenerateBatchFile(string batchFileName, IEnumerable<EarthCoordinate> locations)
        {
            var lowFreqencyDatabase = string.IsNullOrEmpty(NAVOConfiguration.LFBLEXEPath) ? string.Empty : Path.Combine(Path.GetDirectoryName(NAVOConfiguration.LFBLEXEPath), "dbases/");
            var highFreqencyDatabase = string.IsNullOrEmpty(NAVOConfiguration.HFBLEXEPath) ? string.Empty : Path.Combine(Path.GetDirectoryName(NAVOConfiguration.HFBLEXEPath), "dbases/");
            using (var batchFile = new StreamWriter(batchFileName, false))
            {
                foreach (var location in locations)
                {
                    if (!string.IsNullOrEmpty(NAVOConfiguration.LFBLEXEPath))
                    {
                        batchFile.WriteLine("@echo -----LFBL DATA-----");
                        batchFile.WriteLine("@call \"{0}\" \"/\" \"{1}\" {2:0.00} {3:0.00} 1 0", NAVOConfiguration.LFBLEXEPath, lowFreqencyDatabase, location.Latitude, location.Longitude);
                    }
                    if (!string.IsNullOrEmpty(NAVOConfiguration.HFBLEXEPath))
                    {
                        batchFile.WriteLine("@echo -----HFBL DATA-----");
                        batchFile.WriteLine("@call \"{0}\" \"/\" \"{1}\" {2:0.00} {3:0.00}", NAVOConfiguration.HFBLEXEPath, highFreqencyDatabase, location.Latitude, location.Longitude);
                    }
                }
            }
        }

        IEnumerable<string> ExtractorArguments(bool isLowFrequency, IEnumerable<EarthCoordinate> locations) 
        {
            return locations.Select(location => ExtractorArgument(isLowFrequency, location));
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
                throw new ParseException(
                        "Error parsing bottom loss results.  LFBL output not in expected format (latitude)");
            var lfLatitude = Math.Round(double.Parse(fields[1]), 4);
            fields = NextLine(stream).Split(splitCharsSpaceEquals, StringSplitOptions.RemoveEmptyEntries);
            if (fields[0].ToLower() != "longitude")
                throw new ParseException(
                        "Error parsing bottom loss results.  LFBL output not in expected format (longitude)");
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
            if (fields[0].ToLower() != "lat") throw new ParseException("Error parsing bottom loss results.  HFBL output not in expected format (lat)");
            var hfLatitude = Math.Round(double.Parse(fields[1]), 4);
            if (fields[2].ToLower() != "lon") throw new ParseException("Error parsing bottom loss results.  HFBL output not in expected format (lon)");
            var hfLongitude = Math.Round(double.Parse(fields[3]), 4);
            if (curPoint == null) curPoint = new BottomLossSample(hfLatitude, hfLongitude, new BottomLossData());
            else if (((int)(hfLatitude * 10000.0) != (int)(curPoint.Latitude * 10000)) && ((int)(hfLongitude * 10000.0) != (int)(curPoint.Longitude * 10000))) throw new ParseException("Error parsing bottom loss results.  Adjacent LFBL and HFBL extractions do not refer to the same point");
            fields = NextLine(stream).Split(splitCharsCommaEquals, StringSplitOptions.RemoveEmptyEntries);
            if (fields[0].Trim().ToUpper() != "HFBL CURVE NUMBER") throw new ParseException("Error parsing bottom loss results.  HFBL output not in expected format (HFBL curve number)");
            curPoint.Data.CurveNumber = double.Parse(fields[1]);
            return curPoint;
        }

        static EnvironmentData<BottomLossSample> ParseOutput(TextReader stream, bool hasLowFreq, bool hasHighFreq)
        {
            var result = new EnvironmentData<BottomLossSample>();
            var splitCharsSpaceEquals = new[] { ' ', '=' };
            var splitCharsCommaEquals = new[] { ',', '=' };
            var curLine = NextLine(stream);
            while (curLine != null)
            {
                var lfLongitude = 0.0;
                var lfLatitude =  0.0;
                BottomLossSample curPoint = null;
                string[] fields;
                if (hasLowFreq)
                {
                    NextLine(stream, "-----LFBL DATA-----", curLine);
                    fields = NextLine(stream).Split(splitCharsSpaceEquals, StringSplitOptions.RemoveEmptyEntries);
                    if (fields[0].ToLower() != "latitude") throw new ParseException("Error parsing bottom loss results.  LFBL output not in expected format (latitude)");
                    lfLatitude = Math.Round(double.Parse(fields[1]), 4);
                    fields = NextLine(stream).Split(splitCharsSpaceEquals, StringSplitOptions.RemoveEmptyEntries);
                    if (fields[0].ToLower() != "longitude") throw new ParseException("Error parsing bottom loss results.  LFBL output not in expected format (longitude)");
                    lfLongitude = Math.Round(double.Parse(fields[1]), 4);
                    curPoint = new BottomLossSample(lfLatitude, lfLongitude, new BottomLossData());
                    curLine = NextLine(stream, "---- World 15 Parameter set ----");
                    while (!curLine.Contains("Tabular Listing of Parameters"))
                    {
                        fields = NextLine(stream).Split(splitCharsSpaceEquals, StringSplitOptions.RemoveEmptyEntries);
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
                    }
                }

                if (hasHighFreq)
                {
                    NextLine(stream, "-----HFBL DATA-----", curLine);
                    fields = NextLine(stream).Split(splitCharsSpaceEquals, StringSplitOptions.RemoveEmptyEntries);
                    if (fields[0].ToLower() != "lat") throw new ParseException("Error parsing bottom loss results.  HFBL output not in expected format (lat)");
                    var hfLatitude = Math.Round(double.Parse(fields[1]), 4);
                    if (fields[2].ToLower() != "longitude") throw new ParseException("Error parsing bottom loss results.  HFBL output not in expected format (lon)");
                    var hfLongitude = Math.Round(double.Parse(fields[3]), 4);
                    if (curPoint == null) curPoint = new BottomLossSample(hfLatitude, hfLongitude, new BottomLossData());
                    else if ((hfLatitude != lfLatitude) && (hfLongitude != lfLongitude)) throw new ParseException("Error parsing bottom loss results.  Adjacent LFBL and HFBL extractions do not refer to the same point");
                    fields = NextLine(stream).Split(splitCharsCommaEquals, StringSplitOptions.RemoveEmptyEntries);
                    if (fields[0].Trim().ToUpper() != "HFBL CURVE NUMBER") throw new ParseException("Error parsing bottom loss results.  HFBL output not in expected format (HFBL curve number)");
                    curPoint.Data.CurveNumber = double.Parse(fields[1]);
                }
                
                if (curPoint != null) result.Add(curPoint);
                curLine = NextLine(stream);
            }

            return result;
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

    public class BottomLoss
    {
        static readonly List<Type> ReferencedTypes = new List<Type>(EnvironmentData<BottomLossSample>.ReferencedTypes) { typeof(BottomLossSample) };

        public EnvironmentData<BottomLossSample> Samples { get; private set; }

        public BottomLoss()
        {
            Samples = new EnvironmentData<BottomLossSample>();
        }

        public static BottomLoss Load(string filename)
        {
            //return new BottomLoss { Samples = XmlSerializer<EnvironmentData<BottomLossSample>>.Load(filename, ReferencedTypes) };
            var formatter = new BinaryFormatter();
            using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
            {
                return new BottomLoss { Samples = (EnvironmentData<BottomLossSample>)formatter.Deserialize(stream) };
            }
        }

        public void Save(string filename)
        {
            //var serializer = new XmlSerializer<EnvironmentData<BottomLossSample>> { Data = Samples };
            //serializer.Save(filename, ReferencedTypes);
            var formatter = new BinaryFormatter();
            using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None))
            {
                formatter.Serialize(stream, Samples);
            }
        }
    }

    [Serializable]
    public class BottomLossSample : EarthCoordinate<BottomLossData>
    {
        public BottomLossSample() { }

        public BottomLossSample(Geo location, BottomLossData sample) : base(location.Latitude, location.Longitude, sample) { }
        public BottomLossSample(double latitude, double longitude, BottomLossData sample) : base(latitude, longitude, sample) { }
    }

    // ReSharper disable InconsistentNaming
    [Serializable]
    public class BottomLossData 
    {
        public double CurveNumber { get; set; }
        public double RATIOD { get; set; }
        public double DLD { get; set; }
        public double RHOLD { get; set; }
        public double RHOSD { get; set; }
        public double GD { get; set; }
        public double BETAD { get; set; }
        public double FKZD { get; set; }
        public double FKZP { get; set; }
        public double BRFLD { get; set; }
        public double FEXP { get; set; }
        public double D2A { get; set; }
        public double ALF2A { get; set; }
        public double RHO2A { get; set; }
        public double SUBCRIT { get; set; }
        public double T2RH { get; set; }
        public double SEDTHK_M { get; set; }
        public double SEDTHK_S { get; set; }
    }
    // ReSharper restore InconsistentNaming
}
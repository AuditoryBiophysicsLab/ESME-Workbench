using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using Cinch;
using ESME.NEMO;
using GisSharpBlog.NetTopologySuite.IO;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
    public class BottomLossBackgroundExtractor : NAVOBackgroundExtractor
    {
        #region public string LowFrequencyExtractor { get; set; }

        public string LowFrequencyExtractor
        {
            get { return _lowFrequencyExtractor; }
            set
            {
                if (_lowFrequencyExtractor == value) return;
                _lowFrequencyExtractor = value;
                NotifyPropertyChanged(LowFrequencyExtractorChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LowFrequencyExtractorChangedEventArgs = ObservableHelper.CreateArgs<BottomLossBackgroundExtractor>(x => x.LowFrequencyExtractor);
        string _lowFrequencyExtractor;

        #endregion

        #region public string HighFrequencyExtractor { get; set; }

        public string HighFrequencyExtractor
        {
            get { return _highFrequencyExtractor; }
            set
            {
                if (_highFrequencyExtractor == value) return;
                _highFrequencyExtractor = value;
                NotifyPropertyChanged(HighFrequencyExtractorChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs HighFrequencyExtractorChangedEventArgs = ObservableHelper.CreateArgs<BottomLossBackgroundExtractor>(x => x.HighFrequencyExtractor);
        string _highFrequencyExtractor;

        #endregion

        #region public EnvironmentData<BottomLossData> BottomLossData { get; set; }

        public EnvironmentData<BottomLossData> BottomLossData
        {
            get { return _bottomLossData; }
        }

        readonly EnvironmentData<BottomLossData> _bottomLossData = new EnvironmentData<BottomLossData>();

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

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            RunState = "Running";
            TaskName = "Bottom Loss data extraction";
            var backgroundExtractor = (NAVOBackgroundExtractor)e.Argument;
            backgroundExtractor.Status = "Extracting bottom loss data";

            var north = Math.Ceiling(backgroundExtractor.ExtractionArea.North);
            var south = Math.Floor(backgroundExtractor.ExtractionArea.South);
            var east = Math.Ceiling(backgroundExtractor.ExtractionArea.East);
            var west = Math.Floor(backgroundExtractor.ExtractionArea.West);


            var locations = new List<EarthCoordinate>();
            for (var lat = south; lat < north; lat += 0.25)
                for (var lon = west; lon < east; lon += 0.25)
                    locations.Add(new EarthCoordinate(lat, lon));

            
            var useLowFrequency = !string.IsNullOrEmpty(LowFrequencyExtractor);
            var useHighFrequency = !string.IsNullOrEmpty(HighFrequencyExtractor);
            backgroundExtractor.Maximum = locations.Count;
            foreach (var location in locations) 
            {
                BottomLossData curPoint = null;
                if (useLowFrequency)
                {
                    var process = Process.Start(new ProcessStartInfo
                    {
                        Arguments = ExtractorArgument(true, location),
                        FileName = LowFrequencyExtractor,
                        RedirectStandardInput = false,
                        RedirectStandardOutput = true,
                        RedirectStandardError = true,
                        UseShellExecute = false,
                        CreateNoWindow = true,
                    });
                    process.PriorityClass = ProcessPriorityClass.Normal;
                    process.WaitForExit();
                    var stderr = process.StandardError.ReadToEnd();
                    curPoint = ParseLowFrequencyOutput(process.StandardOutput);
                }
                if (useHighFrequency)
                {
                    var process = Process.Start(new ProcessStartInfo
                    {
                        Arguments = ExtractorArgument(false, location),
                        FileName = HighFrequencyExtractor,
                        RedirectStandardInput = false,
                        RedirectStandardOutput = true,
                        RedirectStandardError = true,
                        UseShellExecute = false,
                        CreateNoWindow = true,
                    });
                    process.PriorityClass = ProcessPriorityClass.Normal;
                    process.WaitForExit();
                    var stderr = process.StandardError.ReadToEnd();
                    curPoint = ParseHighFrequencyOutput(process.StandardOutput, curPoint);
                }
                if (curPoint != null) BottomLossData.Add(curPoint);
                backgroundExtractor.Value++;
            }
        }

        void GenerateBatchFile(string batchFileName, IEnumerable<EarthCoordinate> locations)
        {
            var lowFreqencyDatabase = string.IsNullOrEmpty(LowFrequencyExtractor) ? string.Empty : Path.Combine(Path.GetDirectoryName(LowFrequencyExtractor), "dbases/");
            var highFreqencyDatabase = string.IsNullOrEmpty(HighFrequencyExtractor) ? string.Empty : Path.Combine(Path.GetDirectoryName(HighFrequencyExtractor), "dbases/");
            using (var batchFile = new StreamWriter(batchFileName, false))
            {
                foreach (var location in locations)
                {
                    if (!string.IsNullOrEmpty(LowFrequencyExtractor))
                    {
                        batchFile.WriteLine("@echo -----LFBL DATA-----");
                        batchFile.WriteLine("@call \"{0}\" \"/\" \"{1}\" {2:0.00} {3:0.00} 1 0", LowFrequencyExtractor, lowFreqencyDatabase, location.Latitude, location.Longitude);
                    }
                    if (!string.IsNullOrEmpty(HighFrequencyExtractor))
                    {
                        batchFile.WriteLine("@echo -----HFBL DATA-----");
                        batchFile.WriteLine("@call \"{0}\" \"/\" \"{1}\" {2:0.00} {3:0.00}", HighFrequencyExtractor, highFreqencyDatabase, location.Latitude, location.Longitude);
                    }
                }
            }
        }

        IEnumerable<string> ExtractorArguments(bool isLowFrequency, IEnumerable<EarthCoordinate> locations) 
        {
            return locations.Select(location => ExtractorArgument(isLowFrequency, location));
        }

        string ExtractorArgument(bool isLowFrequency, Geo location)
        {
            var database = Path.Combine(isLowFrequency ? Path.GetDirectoryName(LowFrequencyExtractor) : Path.GetDirectoryName(HighFrequencyExtractor), "dbases/");
            return string.Format(isLowFrequency ? " \"/\" \"{0}\" {1:0.00} {2:0.00} 1 0" : " \"/\" \"{0}\" {1:0.00} {2:0.00}", database, location.Latitude, location.Longitude);
        }

        static BottomLossData ParseLowFrequencyOutput(TextReader stream)
        {
            var splitCharsSpaceEquals = new[] { ' ', '=' };

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
            var curPoint = new BottomLossData(lfLatitude, lfLongitude);
            NextLine(stream, "---- World 15 Parameter set ----");
            var curLine = NextLine(stream);
            while (!curLine.Contains("Tabular Listing of Parameters"))
            {
                fields = curLine.Split(splitCharsSpaceEquals, StringSplitOptions.RemoveEmptyEntries);
                switch (fields[0].ToUpper())
                {
                    case "RATIOD":
                        curPoint.RATIOD = double.Parse(fields[1]);
                        break;
                    case "DLD":
                        curPoint.DLD = double.Parse(fields[1]);
                        break;
                    case "RHOLD":
                        curPoint.RHOLD = double.Parse(fields[1]);
                        break;
                    case "RHOSD":
                        curPoint.RHOSD = double.Parse(fields[1]);
                        break;
                    case "GD":
                        curPoint.GD = double.Parse(fields[1]);
                        break;
                    case "BETAD":
                        curPoint.BETAD = double.Parse(fields[1]);
                        break;
                    case "FKZD":
                        curPoint.FKZD = double.Parse(fields[1]);
                        break;
                    case "FKZP":
                        curPoint.FKZP = double.Parse(fields[1]);
                        break;
                    case "BRFLD":
                        curPoint.BRFLD = double.Parse(fields[1]);
                        break;
                    case "FEXP":
                        curPoint.FEXP = double.Parse(fields[1]);
                        break;
                    case "D2A":
                        curPoint.D2A = double.Parse(fields[1]);
                        break;
                    case "ALF2A":
                        curPoint.ALF2A = double.Parse(fields[1]);
                        break;
                    case "RHO2A":
                        curPoint.RHO2A = double.Parse(fields[1]);
                        break;
                    case "SUBCRIT":
                        curPoint.SUBCRIT = double.Parse(fields[1]);
                        break;
                    case "T2RH":
                        curPoint.T2RH = double.Parse(fields[1]);
                        break;
                    case "SEDTHK_M":
                        curPoint.SEDTHK_M = double.Parse(fields[1]);
                        break;
                    case "SEDTHK_S":
                        curPoint.SEDTHK_S = double.Parse(fields[1]);
                        break;
                }
                curLine = NextLine(stream);
            }
            return curPoint;
        }

        static BottomLossData ParseHighFrequencyOutput(TextReader stream, BottomLossData curPoint)
        {
            var splitCharsSpaceEquals = new[] { ' ', '=' };
            var splitCharsCommaEquals = new[] { ',', '=' };

            var fields = NextLine(stream).Split(splitCharsSpaceEquals, StringSplitOptions.RemoveEmptyEntries);
            if (fields[0].ToLower() != "lat") throw new ParseException("Error parsing bottom loss results.  HFBL output not in expected format (lat)");
            var hfLatitude = Math.Round(double.Parse(fields[1]), 4);
            if (fields[2].ToLower() != "lon") throw new ParseException("Error parsing bottom loss results.  HFBL output not in expected format (lon)");
            var hfLongitude = Math.Round(double.Parse(fields[3]), 4);
            if (curPoint == null) curPoint = new BottomLossData(hfLatitude, hfLongitude);
            else if (((int)(hfLatitude * 10000.0) != (int)(curPoint.Latitude * 10000)) && ((int)(hfLongitude * 10000.0) != (int)(curPoint.Longitude * 10000))) throw new ParseException("Error parsing bottom loss results.  Adjacent LFBL and HFBL extractions do not refer to the same point");
            fields = NextLine(stream).Split(splitCharsCommaEquals, StringSplitOptions.RemoveEmptyEntries);
            if (fields[0].Trim().ToUpper() != "HFBL CURVE NUMBER") throw new ParseException("Error parsing bottom loss results.  HFBL output not in expected format (HFBL curve number)");
            curPoint.CurveNumber = double.Parse(fields[1]);
            return curPoint;
        }

        static EnvironmentData<BottomLossData> ParseOutput(TextReader stream, bool hasLowFreq, bool hasHighFreq)
        {
            var result = new EnvironmentData<BottomLossData>();
            var splitCharsSpaceEquals = new[] { ' ', '=' };
            var splitCharsCommaEquals = new[] { ',', '=' };
            var curLine = NextLine(stream);
            while (curLine != null)
            {
                var lfLongitude = 0.0;
                var lfLatitude =  0.0;
                BottomLossData curPoint = null;
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
                    curPoint = new BottomLossData(lfLatitude, lfLongitude);
                    curLine = NextLine(stream, "---- World 15 Parameter set ----");
                    while (!curLine.Contains("Tabular Listing of Parameters"))
                    {
                        fields = NextLine(stream).Split(splitCharsSpaceEquals, StringSplitOptions.RemoveEmptyEntries);
                        switch (fields[0].ToUpper())
                        {
                            case "RATIOD":
                                curPoint.RATIOD = double.Parse(fields[1]);
                                break;
                            case "DLD":
                                curPoint.DLD = double.Parse(fields[1]);
                                break;
                            case "RHOLD":
                                curPoint.RHOLD = double.Parse(fields[1]);
                                break;
                            case "RHOSD":
                                curPoint.RHOSD = double.Parse(fields[1]);
                                break;
                            case "GD":
                                curPoint.GD = double.Parse(fields[1]);
                                break;
                            case "BETAD":
                                curPoint.BETAD = double.Parse(fields[1]);
                                break;
                            case "FKZD":
                                curPoint.FKZD = double.Parse(fields[1]);
                                break;
                            case "FKZP":
                                curPoint.FKZP = double.Parse(fields[1]);
                                break;
                            case "BRFLD":
                                curPoint.BRFLD = double.Parse(fields[1]);
                                break;
                            case "FEXP":
                                curPoint.FEXP = double.Parse(fields[1]);
                                break;
                            case "D2A":
                                curPoint.D2A = double.Parse(fields[1]);
                                break;
                            case "ALF2A":
                                curPoint.ALF2A = double.Parse(fields[1]);
                                break;
                            case "RHO2A":
                                curPoint.RHO2A = double.Parse(fields[1]);
                                break;
                            case "SUBCRIT":
                                curPoint.SUBCRIT = double.Parse(fields[1]);
                                break;
                            case "T2RH":
                                curPoint.T2RH = double.Parse(fields[1]);
                                break;
                            case "SEDTHK_M":
                                curPoint.SEDTHK_M = double.Parse(fields[1]);
                                break;
                            case "SEDTHK_S":
                                curPoint.SEDTHK_S = double.Parse(fields[1]);
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
                    if (curPoint == null) curPoint = new BottomLossData(hfLatitude, hfLongitude);
                    else if ((hfLatitude != lfLatitude) && (hfLongitude != lfLongitude)) throw new ParseException("Error parsing bottom loss results.  Adjacent LFBL and HFBL extractions do not refer to the same point");
                    fields = NextLine(stream).Split(splitCharsCommaEquals, StringSplitOptions.RemoveEmptyEntries);
                    if (fields[0].Trim().ToUpper() != "HFBL CURVE NUMBER") throw new ParseException("Error parsing bottom loss results.  HFBL output not in expected format (HFBL curve number)");
                    curPoint.CurveNumber = double.Parse(fields[1]);
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

    // ReSharper disable InconsistentNaming
    public class BottomLossData : EarthCoordinate<BottomLossData>
    {
        public BottomLossData() { }
        public BottomLossData(double latitude, double longitude) : base(latitude, longitude) { }

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
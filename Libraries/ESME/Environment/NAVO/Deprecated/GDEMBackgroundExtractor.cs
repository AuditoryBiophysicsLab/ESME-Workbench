using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading;
using Cinch;
using HRC.Navigation;
using System.Threading.Tasks;
using HRC.Utility;

namespace ESME.Environment.NAVO
{
#if false
    public class GDEMBackgroundExtractor : NAVOBackgroundExtractor
    {
        #region public SoundSpeedField TemperatureField { get; set; }

        public SoundSpeedField TemperatureField
        {
            get { return _temperatureField; }
            set
            {
                if (_temperatureField == value) return;
                _temperatureField = value;
                NotifyPropertyChanged(TemperatureFieldChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TemperatureFieldChangedEventArgs = ObservableHelper.CreateArgs<GDEMBackgroundExtractor>(x => x.TemperatureField);
        SoundSpeedField _temperatureField;

        #endregion

        #region public SoundSpeedField SalinityField { get; set; }

        public SoundSpeedField SalinityField
        {
            get { return _salinityField; }
            set
            {
                if (_salinityField == value) return;
                _salinityField = value;
                NotifyPropertyChanged(SalinityFieldChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SalinityFieldChangedEventArgs = ObservableHelper.CreateArgs<GDEMBackgroundExtractor>(x => x.SalinityField);
        SoundSpeedField _salinityField;

        #endregion

        #region public EarthCoordinate<float> MaxDepth { get; set; }

        public EarthCoordinate<float> MaxDepth
        {
            get { return _maxDepth; }
            set
            {
                if (_maxDepth == value) return;
                _maxDepth = value;
                NotifyPropertyChanged(MaxDepthChangedEventArgs);
                WaitSemaphore.Release();
            }
        }

        static readonly PropertyChangedEventArgs MaxDepthChangedEventArgs = ObservableHelper.CreateArgs<GDEMBackgroundExtractor>(x => x.MaxDepth);
        EarthCoordinate<float> _maxDepth;

        #endregion

        #region public SoundSpeedField ExtendedSoundSpeedField { get; set; }

        public SoundSpeedField ExtendedSoundSpeedField
        {
            get { return _extendedSoundSpeedField; }
            set
            {
                if (_extendedSoundSpeedField == value) return;
                _extendedSoundSpeedField = value;
                NotifyPropertyChanged(ExtendedSoundSpeedFieldChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExtendedSoundSpeedFieldChangedEventArgs = ObservableHelper.CreateArgs<GDEMBackgroundExtractor>(x => x.ExtendedSoundSpeedField);
        SoundSpeedField _extendedSoundSpeedField;

        #endregion

        #region public string ExtractionProgramPath { get; set; }

        public string ExtractionProgramPath
        {
            get { return _extractionProgramPath; }
            set
            {
                if (_extractionProgramPath == value) return;
                _extractionProgramPath = value;
                NotifyPropertyChanged(ExtractionProgramPathChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs ExtractionProgramPathChangedEventArgs = ObservableHelper.CreateArgs<GDEMBackgroundExtractor>(x => x.ExtractionProgramPath);
        private string _extractionProgramPath;

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

        static readonly PropertyChangedEventArgs PointExtractionModeChangedEventArgs = ObservableHelper.CreateArgs<GDEMBackgroundExtractor>(x => x.PointExtractionMode);
        bool _pointExtractionMode;

        #endregion

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            RunState = "Running";
            var backgroundExtractor = (GDEMBackgroundExtractor)e.Argument;
            TaskName = "Temperature and salinity data extraction for " + backgroundExtractor.TimePeriod;
            backgroundExtractor.Maximum = 6;
            float north, south, east, west;
            if (!PointExtractionMode)
            {
                north = (float)Math.Ceiling(backgroundExtractor.ExtractionArea.North);
                south = (float)Math.Floor(backgroundExtractor.ExtractionArea.South);
                east = (float)Math.Ceiling(backgroundExtractor.ExtractionArea.East);
                west = (float)Math.Floor(backgroundExtractor.ExtractionArea.West);
            }
            else
            {
                north = (float)backgroundExtractor.ExtractionArea.North;
                south = (float)backgroundExtractor.ExtractionArea.South;
                east = (float)backgroundExtractor.ExtractionArea.East;
                west = (float)backgroundExtractor.ExtractionArea.West;
            }
            var commandArgs = string.Format("-out \"{0}\" -gdem \"{1}\" -months {2} -north {3} -south {4} -east {5} -west {6} -new", backgroundExtractor.DestinationPath, backgroundExtractor.NAVOConfiguration.GDEMDirectory, backgroundExtractor.TimePeriod, north, south, east, west);

            NAVOExtractionProgram.Execute(backgroundExtractor.ExtractionProgramPath, commandArgs, backgroundExtractor.DestinationPath);
            backgroundExtractor.Value++;

            var temperatureFileName = Path.Combine(backgroundExtractor.DestinationPath, string.Format("{0}.temperature", backgroundExtractor.TimePeriod));
            var salinityFileName = Path.Combine(backgroundExtractor.DestinationPath, string.Format("{0}.salinity", backgroundExtractor.TimePeriod));

            var field = SoundSpeed.Load(temperatureFileName);
            File.Delete(temperatureFileName);
            TemperatureField = field.SoundSpeedFields[0];
            backgroundExtractor.Value++;

            field = SoundSpeed.Load(salinityFileName);
            File.Delete(salinityFileName);
            SalinityField = field.SoundSpeedFields[0];
            backgroundExtractor.Value++;

            TaskName = "Soundspeed calculation for " + TimePeriod;
            RunState = "Waiting for bathymetry";
            //Console.WriteLine("GDEM: Waiting on bathymetry for " + TimePeriod);
            WaitSemaphore.WaitOne();
            RunState = "Running";
            backgroundExtractor.Status = "Creating soundspeed profile for " + TimePeriod;

            var soundSpeedField = SoundSpeedField.Create(TemperatureField, SalinityField);
            backgroundExtractor.Value++;

            backgroundExtractor.Status = "Extending soundspeed profile for " + TimePeriod;
            soundSpeedField.Extend(TemperatureField, SalinityField, MaxDepth);
            backgroundExtractor.Value++;
            ExtendedSoundSpeedField = soundSpeedField;
            if (!backgroundExtractor.UseExpandedExtractionArea)
            {
                backgroundExtractor.Status = "Trimming soundspeed data for " + TimePeriod;
                TemperatureField.EnvironmentData.TrimToNearestPoints(backgroundExtractor.ExtractionArea);
                SalinityField.EnvironmentData.TrimToNearestPoints(backgroundExtractor.ExtractionArea);
                ExtendedSoundSpeedField.EnvironmentData.TrimToNearestPoints(backgroundExtractor.ExtractionArea);
            }
            backgroundExtractor.Value++;
        }

        public static void ComputeSoundSpeeds(string filePath, SoundSpeed temperature, SoundSpeed salinity, EarthCoordinate<float> maxDepth, IEnumerable<NAVOTimePeriod> timePeriods, IProgress<TaskProgressInfo> progress = null)
        {
            var taskProgress = new TaskProgressInfo
            {
                TaskName = "SoundSpeed Creation",
                CurrentActivity = "Initializing",
                ProgressPercent = 0,
            };
            if (progress != null) progress.Report(taskProgress);
            var soundSpeed = new SoundSpeed();
            for (var i = 0; i < temperature.SoundSpeedFields.Count; i++)
            {
                var temperatureField = temperature.SoundSpeedFields[i];
                var salinityField = salinity[temperatureField.TimePeriod];
                var soundSpeedField = SoundSpeedField.Create(temperatureField, salinityField);
                soundSpeedField.Extend(temperatureField, salinityField, maxDepth);
                soundSpeed.SoundSpeedFields.Add(soundSpeedField);
            }
            foreach (var timePeriod in timePeriods)
            {
                var timePeriodCount = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(timePeriod).ToList().Count();
                if (timePeriodCount > 1) soundSpeed.SoundSpeedFields.Add(SoundSpeed.Average(soundSpeed, timePeriod));
            }
            soundSpeed.Save(filePath + ".soundspeed");
        }

        public static void ExtractAsync(bool isPointExtraction, float north, float south, float east, float west, IEnumerable<NAVOTimePeriod> timePeriods, string outputFilename, IProgress<TaskProgressInfo> progress = null)
        {
            var taskProgress = new TaskProgressInfo
            {
                TaskName = "GDEM Extraction",
                CurrentActivity = "Initializing",
                ProgressPercent = 0,
            };
            if (progress != null) progress.Report(taskProgress);
            
            if (!isPointExtraction)
            {
                north = (float)Math.Ceiling(north);
                south = (float)Math.Floor(south);
                east = (float)Math.Ceiling(east);
                west = (float)Math.Floor(west);
            }

            var assemblyLocation = Assembly.GetCallingAssembly().Location;
            var extractionPath = Path.GetDirectoryName(assemblyLocation);
            if (extractionPath == null) throw new ApplicationException("Extraction path can't be null!");
            var gdemExtractionProgramPath = Path.Combine(extractionPath, "ImportGDEM.exe");
            var tempPath = Path.GetDirectoryName(outputFilename);
            if (!Directory.Exists(tempPath)) Directory.CreateDirectory(tempPath);
            var sb = new StringBuilder();
            foreach (var timePeriod in timePeriods) sb.Append(string.Format("{0},", timePeriod.ToString().ToLower()));
            sb.Remove(sb.Length - 1, 1);

            var commandArgs = string.Format("-out \"{0}\" -gdem \"{1}\" -months {2} -north {3} -south {4} -east {5} -west {6}", outputFilename, Globals.AppSettings.NAVOConfiguration.GDEMDirectory, sb, north, south, east, west);

            taskProgress.CurrentActivity = "Waiting for extraction program to complete";
            taskProgress.ProgressPercent = 5;
            if (progress != null) progress.Report(taskProgress);
            var result = NAVOExtractionProgram.Execute(gdemExtractionProgramPath, commandArgs, tempPath);
            taskProgress.CurrentActivity = "Done";
            taskProgress.ProgressPercent = 100;
            if (progress != null) progress.Report(taskProgress);
        }
    }
#endif
}
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using Cinch;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
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

        #region public List<string> RequiredSupportFiles { get; set; }

        public List<string> RequiredSupportFiles
        {
            get { return _requiredSupportFiles; }
            set
            {
                if (_requiredSupportFiles == value) return;
                _requiredSupportFiles = value;
                NotifyPropertyChanged(RequiredSupportFilesChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs RequiredSupportFilesChangedEventArgs = ObservableHelper.CreateArgs<GDEMBackgroundExtractor>(x => x.RequiredSupportFiles);
        private List<string> _requiredSupportFiles;

        #endregion

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            RunState = "Running";
            var backgroundExtractor = (GDEMBackgroundExtractor)e.Argument;
            TaskName = "Temperature and salinity data extraction for " + backgroundExtractor.TimePeriod;
            backgroundExtractor.Maximum = 6;
            var north = (int)Math.Ceiling(backgroundExtractor.ExtractionArea.North);
            var south = (int)Math.Floor(backgroundExtractor.ExtractionArea.South);
            var east = (int)Math.Ceiling(backgroundExtractor.ExtractionArea.East);
            var west = (int)Math.Floor(backgroundExtractor.ExtractionArea.West);

            var commandArgs = string.Format("-out \"{0}\" -gdem \"{1}\" -months {2} -north {3} -south {4} -east {5} -west {6} -new", backgroundExtractor.DestinationPath, backgroundExtractor.NAVOConfiguration.GDEMDirectory, backgroundExtractor.TimePeriod, north, south, east, west);

            NAVOExtractionProgram.Execute(backgroundExtractor.ExtractionProgramPath, commandArgs, backgroundExtractor.DestinationPath, backgroundExtractor.RequiredSupportFiles);
            backgroundExtractor.Value++;

            var temperatureFileName = Path.Combine(backgroundExtractor.DestinationPath, string.Format("{0}-temperature.xml", backgroundExtractor.TimePeriod));
            var salinityFileName = Path.Combine(backgroundExtractor.DestinationPath, string.Format("{0}-salinity.xml", backgroundExtractor.TimePeriod));

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
    }
}
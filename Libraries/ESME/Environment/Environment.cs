using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.IO;
using System.Linq;
using Cinch;
using ESME.Environment.NAVO;
using ESME.TransmissionLoss.CASS;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment
{
    public class Environment : PropertyChangedBase
    {
        #region public Bathymetry Bathymetry { get; set; }

        public Bathymetry Bathymetry
        {
            get { return _bathymetry; }
            set
            {
                if (_bathymetry == value) return;
                _bathymetry = value;
                NotifyPropertyChanged(BathymetryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BathymetryChangedEventArgs = ObservableHelper.CreateArgs<Environment>(x => x.Bathymetry);
        Bathymetry _bathymetry;

        #endregion

        #region public Sediment Sediment { get; set; }

        public Sediment Sediment
        {
            get { return _sediment; }
            set
            {
                if (_sediment == value) return;
                _sediment = value;
                NotifyPropertyChanged(SedimentChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SedimentChangedEventArgs = ObservableHelper.CreateArgs<Environment>(x => x.Sediment);
        Sediment _sediment;

        #endregion

        #region public SoundSpeed SoundSpeed { get; set; }

        public SoundSpeed SoundSpeed
        {
            get { return _soundSpeed; }
            set
            {
                if (_soundSpeed == value) return;
                _soundSpeed = value;
                NotifyPropertyChanged(SoundSpeedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SoundSpeedChangedEventArgs = ObservableHelper.CreateArgs<Environment>(x => x.SoundSpeed);
        SoundSpeed _soundSpeed;

        #endregion

        #region public SoundSpeed Salinity { get; set; }

        public SoundSpeed Salinity
        {
            get { return _salinity; }
            set
            {
                if (_salinity == value) return;
                _salinity = value;
                NotifyPropertyChanged(SalinityChangedEventArgs);
                ComputeSoundSpeedIfNeeded();
            }
        }

        static readonly PropertyChangedEventArgs SalinityChangedEventArgs = ObservableHelper.CreateArgs<Environment>(x => x.Salinity);
        SoundSpeed _salinity;

        #endregion

        #region public SoundSpeed Temperature { get; set; }

        public SoundSpeed Temperature
        {
            get { return _temperature; }
            set
            {
                if (_temperature == value) return;
                _temperature = value;
                NotifyPropertyChanged(TemperatureChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TemperatureChangedEventArgs = ObservableHelper.CreateArgs<Environment>(x => x.Temperature);
        SoundSpeed _temperature;

        #endregion

        #region public Wind Wind { get; set; }

        public Wind Wind
        {
            get { return _wind; }
            set
            {
                if (_wind == value) return;
                _wind = value;
                NotifyPropertyChanged(WindChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WindChangedEventArgs = ObservableHelper.CreateArgs<Environment>(x => x.Wind);
        Wind _wind;

        #endregion

        void ComputeSoundSpeedIfNeeded()
        {
            if ((SoundSpeed == null) && (Temperature != null) && (Salinity != null))
            {
                SoundSpeed = SoundSpeed.Create(Temperature, Salinity);
            }
        }

        public void Export(string simAreaPath, IEnumerable<NAVOTimePeriod> timePeriods, GeoRect areaToExport = null, BackgroundTask backgroundTask = null)
        {
            ExportBathymetry(simAreaPath, areaToExport, backgroundTask);
            if ((backgroundTask != null) && backgroundTask.CancellationPending) return;
            //ExportEnvironment(simAreaPath, timePeriods, areaToExport, backgroundTask);
        }

        public static int EnvironmentExportStepCount(IEnumerable<NAVOTimePeriod> selectedTimePeriods)
        {
            var selectedMonthIndices = new List<NAVOTimePeriod>();
            var averagedTimePeriods = new List<NAVOTimePeriod>();
            foreach (var timePeriod in selectedTimePeriods)
            {
                var monthsInTimePeriod = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(timePeriod);
                selectedMonthIndices.AddRange(monthsInTimePeriod);
                if (monthsInTimePeriod.Count() > 1) averagedTimePeriods.Add(timePeriod);
            }
            var uniqueMonths = selectedMonthIndices.Distinct().ToList();
            uniqueMonths.Sort();
            var result = uniqueMonths.Count;
            result += averagedTimePeriods.Count;
            result += selectedTimePeriods.Count();
            return result;
        }
#if false
        public void ExportEnvironment(string simAreaPath, IEnumerable<NAVOTimePeriod> timePeriods, GeoRect areaToExport = null, BackgroundTask backgroundTask = null)
        {
            if ((Bathymetry == null) || (Sediment == null) || (SoundSpeed == null) || (Salinity == null) || (Temperature == null) || (Wind == null)) throw new DataException("Unable to export environmental data: One or more required data types are not present");
            
            var selectedBathymetry = Bathymetry;

            if (areaToExport != null)
            {
                if (!Bathymetry.Samples.GeoRect.Contains(areaToExport)) throw new DataException("Unable to export environmental data: The requested area to export is not contained within the bounds of the available bathymetry data");
                if (!Bathymetry.Samples.GeoRect.Equals(areaToExport))
                {
                    selectedBathymetry = new Bathymetry();
                    selectedBathymetry.Samples.AddRange(Bathymetry.Samples);
                    selectedBathymetry.Samples.TrimToNearestPoints(areaToExport);
                    if ((backgroundTask != null) && backgroundTask.CancellationPending) return;
                }
            }
            else areaToExport = selectedBathymetry.Samples.GeoRect;

            var deepestPoint = new EarthCoordinate<float>(Bathymetry.Minimum, Math.Abs(Bathymetry.Minimum.Data));
            
            var selectedMonthIndices = new List<NAVOTimePeriod>();
            var averagedTimePeriods = new List<NAVOTimePeriod>();
            foreach (var timePeriod in timePeriods)
            {
                var monthsInTimePeriod = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(timePeriod);
                selectedMonthIndices.AddRange(monthsInTimePeriod);
                if (monthsInTimePeriod.Count() > 1) averagedTimePeriods.Add(timePeriod);
            }
            var uniqueMonths = selectedMonthIndices.Distinct().ToList();
            uniqueMonths.Sort();

            var extendedAndAveragedSoundSpeeds = new SoundSpeed();
            foreach (var month in uniqueMonths)
            {
                if (backgroundTask != null) backgroundTask.Status = string.Format("Extending soundspeeds for {0}", month);
                extendedAndAveragedSoundSpeeds.SoundSpeedFields.Add(SoundSpeed[month].Extend(Temperature[month], Salinity[month], deepestPoint, areaToExport));
                if (backgroundTask != null) backgroundTask.Value++;
                if ((backgroundTask != null) && backgroundTask.CancellationPending) return;
            }
            
            if (averagedTimePeriods.Count() > 0)
                extendedAndAveragedSoundSpeeds.Add(SoundSpeed.Average(extendedAndAveragedSoundSpeeds, averagedTimePeriods, backgroundTask));
            if ((backgroundTask != null) && backgroundTask.CancellationPending) return;

            foreach (var timePeriod in timePeriods)
            {
                if (backgroundTask != null) backgroundTask.Status = string.Format("Exporting environment data for {0}", timePeriod);
                var environmentFileName = Path.Combine(Path.Combine(simAreaPath, "Environment"), "env_" + timePeriod.ToString().ToLower() + ".dat");
                CASSFiles.WriteEnvironmentFile(environmentFileName, selectedBathymetry.Samples.GeoRect, Sediment, extendedAndAveragedSoundSpeeds[timePeriod], Wind[timePeriod]);
                if (backgroundTask != null) backgroundTask.Value++;
                if ((backgroundTask != null) && backgroundTask.CancellationPending) return;
            }
        }
#endif

        public void ExportBathymetry(string simAreaPath, GeoRect areaToExport = null, BackgroundTask backgroundTask = null)
        {
            if (Bathymetry == null) throw new DataException("Unable to export bathymetry data: No bathymetry data is present");

            var selectedBathymetry = Bathymetry;

            if (areaToExport != null)
            {
                if (!Bathymetry.Samples.GeoRect.Contains(areaToExport)) throw new DataException("Unable to export environmental data: The requested area to export is not contained within the bounds of the available bathymetry data");
                if (!Bathymetry.Samples.GeoRect.Equals(areaToExport))
                {
                    selectedBathymetry = new Bathymetry();
                    selectedBathymetry.Samples.AddRange(Bathymetry.Samples);
                    selectedBathymetry.Samples.TrimToNearestPoints(areaToExport);
                    if ((backgroundTask != null) && backgroundTask.CancellationPending) return;
                }
            }

            if (backgroundTask != null) backgroundTask.Status = string.Format("Exporting bathymetry data");
            selectedBathymetry.ToYXZ(Path.Combine(simAreaPath, "Bathymetry", "bathymetry.txt"), -1);
        }
    }
}

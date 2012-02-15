using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using Cinch;
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Plugins;
using HRC.Navigation;
using HRC.Validation;
using Microsoft.Win32;

namespace NAVO
{
    public sealed class GDEM3 : EnvironmentalDataSourcePluginBase<SoundSpeed>, IGDEM3DataSource<SoundSpeed>
    {
        static readonly string[] RequiredGDEMFilenames = 
        { "sgdemv3s01.nc", "sgdemv3s02.nc", "sgdemv3s03.nc", "sgdemv3s04.nc", "sgdemv3s05.nc", "sgdemv3s06.nc", 
          "sgdemv3s07.nc", "sgdemv3s08.nc", "sgdemv3s09.nc", "sgdemv3s10.nc", "sgdemv3s11.nc", "sgdemv3s12.nc",
          "tgdemv3s01.nc", "tgdemv3s02.nc", "tgdemv3s03.nc", "tgdemv3s04.nc", "tgdemv3s05.nc", "tgdemv3s06.nc",
          "tgdemv3s07.nc", "tgdemv3s08.nc", "tgdemv3s09.nc", "tgdemv3s10.nc", "tgdemv3s11.nc", "tgdemv3s12.nc"
        };

        public GDEM3()
        {
            PluginName = "GDEM v3";
            PluginDescription = "Generalized Digital Environment Model v3, from US Navy/NAVOCEANO";
            DataLocationHelp = "A directory containing the GDEM-V 3.0 NetCDF files with temperature and salinity data (tgdemv3s01.nc for example)";
            PluginType = PluginType.EnvironmentalDataSource;
            Resolutions = new float[] { 4 };
            var regKey = Registry.LocalMachine.OpenSubKey(@"Software\Boston University\ESME Workbench\Data Sources\GDEM-V 3.0");
            if (regKey != null) DataLocation = (string)regKey.GetValue("");
            else DataLocation = null;

            IsAvailable = DataLocation != null;

            ValidationRules.AddRange(new List<ValidationRule>
            {
                new ValidationRule
                {
                    PropertyName = "DataLocation",
                    Description = "Directory must exist and contain 24 appropriate GDEM NetCDF files (names like [t|s]gdemv3s[01-12].nc)",
                    RuleDelegate = (o, r) => ((GDEM3)o).DataLocation != null && Directory.Exists(((GDEM3)o).DataLocation) && RequiredGDEMFilenames.All(requiredFile => File.Exists(Path.Combine(DataLocation, requiredFile))),
                },
            });
        }

        #region public string DataLocation { get; set; }

        public override string DataLocation
        {
            get { return _dataLocation; }
            set
            {
                if (_dataLocation == value) return;
                _dataLocation = value;
                if (_dataLocation != null)
                {
                    var attr = File.GetAttributes(_dataLocation);
                    if ((attr & FileAttributes.Directory) != FileAttributes.Directory) _dataLocation = Path.GetDirectoryName(_dataLocation);
                }
                NotifyPropertyChanged(DataLocationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DataLocationChangedEventArgs = ObservableHelper.CreateArgs<GDEM3>(x => x.DataLocation);
        string _dataLocation;
        #endregion

        public SoundSpeed ExtractTemperature(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, IProgress<float> progress = null)
        {
            if (progress != null) lock (progress) progress.Report(0f);
            var temperatureField = GDEM.ReadFile(GDEM.FindTemperatureFile(timePeriod), "water_temp", timePeriod, geoRect);
            if (progress != null) lock (progress) progress.Report(50f);
            var temperature = new SoundSpeed();
            temperature.SoundSpeedFields.Add(temperatureField);
            if (progress != null) lock (progress) progress.Report(100f);
            return temperature;
        }

        public SoundSpeed ExtractSalinity(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, IProgress<float> progress = null)
        {
            if (progress != null) lock (progress) progress.Report(0f);
            var salinityField = GDEM.ReadFile(GDEM.FindSalinityFile(timePeriod), "salinity", timePeriod, geoRect);
            if (progress != null) lock (progress) progress.Report(50f);
            var salinity = new SoundSpeed();
            salinity.SoundSpeedFields.Add(salinityField);
            if (progress != null) lock (progress) progress.Report(100f);
            return salinity;
        }

        public override SoundSpeed Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, IProgress<float> progress = null)
        {
            throw new NotImplementedException("GDEM v3 extraction routine requires either deepest point OR bathymetry argument");
        }

        public SoundSpeed Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, EarthCoordinate<float> deepestPoint, IProgress<float> progress = null)
        {
            if (progress != null) lock (progress) progress.Report(0);
            var curProgressStep = 0f;
            var intermediateProgress = new Progress<float>(prog =>
            {
                if (progress != null) lock (progress) progress.Report(curProgressStep + (prog / 3));
            });
            var temperatureData = ExtractTemperature(geoRect, resolution, timePeriod, intermediateProgress);
            curProgressStep = 33f;
            var salinityData = ExtractSalinity(geoRect, resolution, timePeriod, intermediateProgress);
            curProgressStep = 66f;
            var result = SoundSpeed.Create(temperatureData, salinityData, deepestPoint, intermediateProgress);
            if (progress != null) lock (progress) progress.Report(100f);
            return result;
        }

        public SoundSpeed Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, Bathymetry bathymetry, IProgress<float> progress = null) 
        {
            return Extract(geoRect, resolution, timePeriod, bathymetry.DeepestPoint);
        }
    }
}

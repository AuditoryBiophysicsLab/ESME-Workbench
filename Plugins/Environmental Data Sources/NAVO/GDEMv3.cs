using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Windows.Controls;
using Cinch;
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
using HRC.Navigation;
using HRC.Validation;
using ValidationRule = HRC.Validation.ValidationRule;

namespace GDEMv3
{
    public sealed class GDEM3 : ValidatingViewModel, IGDEM3DataSource
    {
        static readonly string[] RequiredGDEMFilenames = 
        { "sgdemv3s01.nc", "sgdemv3s02.nc", "sgdemv3s03.nc", "sgdemv3s04.nc", "sgdemv3s05.nc", "sgdemv3s06.nc", 
          "sgdemv3s07.nc", "sgdemv3s08.nc", "sgdemv3s09.nc", "sgdemv3s10.nc", "sgdemv3s11.nc", "sgdemv3s12.nc",
          "tgdemv3s01.nc", "tgdemv3s02.nc", "tgdemv3s03.nc", "tgdemv3s04.nc", "tgdemv3s05.nc", "tgdemv3s06.nc",
          "tgdemv3s07.nc", "tgdemv3s08.nc", "tgdemv3s09.nc", "tgdemv3s10.nc", "tgdemv3s11.nc", "tgdemv3s12.nc",
        };

        public GDEM3() 
        {
            PluginName = "GDEM v3";
            PluginDescription = "Generalized Digital Environment Model v3, from US Navy/NAVOCEANO";
            DataLocationHelp = "A directory containing the 24 GDEMv3 NetCDF files (sgdemv3s01.nc for example)";
            ConfigurationControl = new ConfigurationControl {DataContext = this};
            PluginType = PluginType.EnvironmentalDataSource;

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

        public string PluginName { get; private set; }
        public string PluginDescription { get; private set; }
        public string DataLocationHelp { get; private set; }
        public Control ConfigurationControl { get; private set; }
        public PluginType PluginType { get; private set; }
        /// <summary>
        /// An array of available resolutions in samples per degree
        /// </summary>
        public float[] Resolutions { get { return new float[] { 4 }; } }

        #region public string DataLocation { get; set; }

        public string DataLocation
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

        #region public bool IsDataLocationValid { get; set; }

        public bool IsDataLocationValid
        {
            get { return _isDataLocationValid; }
            set
            {
                _isDataLocationValid = value;
                NotifyPropertyChanged(IsDataLocationValidChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsDataLocationValidChangedEventArgs = ObservableHelper.CreateArgs<GDEM3>(x => x.IsDataLocationValid);
        bool _isDataLocationValid;

        #endregion

        public SoundSpeedField ExtractTemperature(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod) { throw new NotImplementedException(); }
        public SoundSpeedField ExtractSalinity(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod) { throw new NotImplementedException(); }
        public SoundSpeedField Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod) { throw new NotImplementedException(); }
    }
}

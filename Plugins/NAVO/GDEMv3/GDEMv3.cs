using System;
using System.ComponentModel;
using System.IO;
using System.Linq;
using ESME.Environment;
using ESME.Environment.NAVO;
using HRC.Navigation;
using HRC.Utility;

namespace GDEMv3
{
    public class GDEM3 : PropertyChangedBase, IGDEM3DataSource
    {
        public string PluginName { get { return "GDEM v3"; } }

        public string PluginDescription { get { return "Generalized Digital Environment Model v3, from US Navy/NAVOCEANO"; } }

        /// <summary>
        /// An array of available resolutions in samples per degree
        /// </summary>
        public float[] Resolutions { get { return new float[] {4}; } }

        #region public string DataLocation { get; set; }

        public string DataLocation
        {
            get { return _dataLocation; }
            set
            {
                if (_dataLocation == value) return;
                _dataLocation = value;
                NotifyPropertyChanged(DataLocationChangedEventArgs);
                if (string.IsNullOrEmpty(_dataLocation) || !Directory.Exists(_dataLocation))
                {
                    IsDataLocationValid = false;
                    return;
                }
                if (RequiredFilenames.Any(requiredFile => !File.Exists(Path.Combine(_dataLocation, requiredFile))))
                    IsDataLocationValid = false;
            }
        }

        static readonly PropertyChangedEventArgs DataLocationChangedEventArgs = CreateArgs<GDEM3>(x => x.DataLocation);
        string _dataLocation;
        static readonly string[] RequiredFilenames = 
        {
            "sgdemv3s01.nc",
            "sgdemv3s02.nc",
            "sgdemv3s03.nc",
            "sgdemv3s04.nc",
            "sgdemv3s05.nc",
            "sgdemv3s06.nc",
            "sgdemv3s07.nc",
            "sgdemv3s08.nc",
            "sgdemv3s09.nc",
            "sgdemv3s10.nc",
            "sgdemv3s11.nc",
            "sgdemv3s12.nc",
            "tgdemv3s01.nc",
            "tgdemv3s02.nc",
            "tgdemv3s03.nc",
            "tgdemv3s04.nc",
            "tgdemv3s05.nc",
            "tgdemv3s06.nc",
            "tgdemv3s07.nc",
            "tgdemv3s08.nc",
            "tgdemv3s09.nc",
            "tgdemv3s10.nc",
            "tgdemv3s11.nc",
            "tgdemv3s12.nc",
        };
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

        static readonly PropertyChangedEventArgs IsDataLocationValidChangedEventArgs = CreateArgs<GDEM3>(x => x.IsDataLocationValid);
        bool _isDataLocationValid;

        #endregion

        public string DataLocationHelp { get { return "A directory containing the 24 GDEMv3 NetCDF files (sgdemv3s01.nc for example)"; } }

        public SoundSpeedField ExtractTemperature(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod) { throw new NotImplementedException(); }
        public SoundSpeedField ExtractSalinity(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod) { throw new NotImplementedException(); }
        public SoundSpeedField Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod) { throw new NotImplementedException(); }
    }
}

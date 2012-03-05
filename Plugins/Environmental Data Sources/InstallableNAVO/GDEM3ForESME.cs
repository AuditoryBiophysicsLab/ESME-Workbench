using System;
using System.IO;
using System.Linq;
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Plugins;
using HRC.Navigation;
using Microsoft.Win32;
using NAVODatabaseAdapter;

namespace InstallableNAVO
{
    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource,
        Subtype = "Sound Speed",
        Name = "GDEM-V 3.0 for ESME Workbench",
        Description = "Generalized Digital Environment Model, Variable Resolution version 3.0 from US Navy/NAVOCEANO, packaged for ESME Workbench")]
    public sealed class GDEM3ForESME : EnvironmentalDataSourcePluginBase<SoundSpeed>
    {
        public GDEM3ForESME()
        {
            SetPropertiesFromAttributes(GetType());
            IsTimeVariantData = true;
            AvailableTimePeriods = NAVOConfiguration.AllMonths.ToArray();
            AvailableResolutions = new float[] { 15 };
            var regKey = Registry.LocalMachine.OpenSubKey(@"Software\Boston University\ESME Workbench\Data Sources\GDEM-V 3.0");
            if (regKey != null) _dataDirectory = (string)regKey.GetValue("");

            IsSelectable = _dataDirectory != null;
            IsConfigured = _dataDirectory != null &&
                           Directory.Exists(_dataDirectory) &&
                           GDEM.IsDirectoryValid(_dataDirectory);
        }

        readonly string _dataDirectory;

        public override SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            var result = new SoundSpeed();
            result.Add(GDEM.ReadFile(_dataDirectory, timePeriod, geoRect));
            return result;
        }
    }
}
using System;
using System.Collections.Generic;
using System.IO;
using Cinch;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Environment.NAVO;
using ESME.Locations;
using ESME.Plugins;
using ESME.Scenarios;
using HRC.Aspects;
using HRC.Validation;

namespace ESME.Views.Locations
{
    [NotifyPropertyChanged]
    public class ImportScenarioFileViewModel : ValidatingViewModel
    {
        public ImportScenarioFileViewModel(MasterDatabaseService database, EnvironmentalCacheService cache, IPluginManagerService plugins)
        {
            _database = database;
            _cache = cache;
            _plugins = plugins;
            ValidationRules.AddRange(new List<ValidationRule> { ScenarioFilenameRule, SimAreaCSVFilenameRule });
        }

        readonly EnvironmentalCacheService _cache;
        readonly MasterDatabaseService _database;
        readonly IPluginManagerService _plugins;
        public string ScenarioFilename { get; set; }

        public string SimAreaCSVFilename { get; set; }
        public Scenario Scenario { get; set; }
        #region Validation Rules
        static readonly ValidationRule ScenarioFilenameRule = new ValidationRule
        {
            PropertyName = "ScenarioFilename",
            Description = "File must exist",
            RuleDelegate = (o, r) =>
            {
                var target = (ImportScenarioFileViewModel)o;
                return File.Exists(target.ScenarioFilename);
            },
        };
        static readonly ValidationRule SimAreaCSVFilenameRule = new ValidationRule
        {
            PropertyName = "SimAreaCSVFilename",
            Description = "File must exist",
            RuleDelegate = (o, r) =>
            {
                var target = (ImportScenarioFileViewModel)o;
                return File.Exists(target.SimAreaCSVFilename);
            },
        };
        #endregion
        #region OkCommand
        public SimpleCommand<object, object> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, object>(delegate { return IsOkCommandEnabled; }, delegate { OkHandler(); })); }
        }

        SimpleCommand<object, object> _ok;

        bool IsOkCommandEnabled
        {
            get { return IsValid; }
        }

        void OkHandler()
        {
            var simAreaFolder = Path.GetDirectoryName(SimAreaCSVFilename);
            var nemoFile = new NEMO.NemoFile(ScenarioFilename, simAreaFolder);
            var simAreaOverlayFile = FindSimAreaOverlayFilename(nemoFile.Scenario.SimAreaName);
            var rangeComplexDirectory = Path.Combine(simAreaFolder, nemoFile.Scenario.SimAreaName);
            var location = _database.ImportLocationFromOverlayFile(Path.Combine(rangeComplexDirectory, "Areas", simAreaOverlayFile), nemoFile.Scenario.SimAreaName);
            foreach (var month in NAVOConfiguration.AllMonths)
            {
                // SoundSpeed dataset for each month
                Console.WriteLine(string.Format("Importing soundspeed for {0}", month));
                _cache.ImportDataset(_database.CreateEnvironmentalDataSet(location, 15, month, _plugins[PluginType.EnvironmentalDataSource, PluginSubtype.SoundSpeed].PluginIdentifier));

                // Wind dataset for each month
                Console.WriteLine(string.Format("Importing wind for {0}", month));
                _cache.ImportDataset(_database.CreateEnvironmentalDataSet(location, 60, month, _plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Wind].PluginIdentifier));
            }
            // Sediment dataset
            Console.WriteLine("Importing sediment");
            _cache.ImportDataset(_database.CreateEnvironmentalDataSet(location, 5f, TimePeriod.Invalid, _plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Sediment].PluginIdentifier));

            // Bathymetry dataset at 2min resolution
            Console.WriteLine("Importing 2min bathymetry");
            _cache.ImportDataset(_database.CreateEnvironmentalDataSet(location, 2f, TimePeriod.Invalid, _plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry].PluginIdentifier));
            // Bathymetry dataset at 1min resolution
            Console.WriteLine("Importing 1min bathymetry");
            _cache.ImportDataset(_database.CreateEnvironmentalDataSet(location, 1f, TimePeriod.Invalid, _plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry].PluginIdentifier));
            // Bathymetry dataset at 0.5min resolution
            Console.WriteLine("Importing 0.5min bathymetry");
            _cache.ImportDataset(_database.CreateEnvironmentalDataSet(location, 0.5f, TimePeriod.Invalid, _plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry].PluginIdentifier));
            Scenario = Scenario.FromNemoFile(_database, location, ScenarioFilename, simAreaFolder); 
            CloseActivePopUpCommand.Execute(true);
        }
        #endregion

        #region CancelCommand
        public SimpleCommand<object, object> CancelCommand
        {
            get { return _cancel ?? (_cancel = new SimpleCommand<object, object>(delegate { return IsCancelCommandEnabled; }, delegate { CancelHandler(); })); }
        }

        SimpleCommand<object, object> _cancel;

        bool IsCancelCommandEnabled
        {
            get { return true; }
        }

        void CancelHandler()
        {
            CloseActivePopUpCommand.Execute(false);

        }
        #endregion

        string FindSimAreaOverlayFilename(string simAreaName)
        {
            var lines = File.ReadAllLines(SimAreaCSVFilename);
            foreach (var line in lines)
            {
                if (line == null) throw new ApplicationException("line is null");
                var curLine = line.Trim();
                if ((curLine.Trim() == "") || curLine.StartsWith("!") || curLine.StartsWith("#")) continue;
                var fields = curLine.Split(',');
                var rangeComplexName = fields[0].Trim();
                var simLimitFile = fields[6].Trim();
                if (rangeComplexName == simAreaName) return simLimitFile;
            }
            return null;
        }
    }
}

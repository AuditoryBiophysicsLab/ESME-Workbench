using System;
using System.Data.Entity;
using System.IO;
using System.Linq;
using Cinch;
using ESME.Locations;
using ESME.Plugins;
using ESME.Scenarios;
using HRC.Aspects;
using HRC.Utility;
using MEFedMVVM.ViewModelLocator;
using System.ComponentModel.Composition;

namespace DavesWPFTester
{
    [ExportViewModel("MainWindowViewModel")]
    [NotifyPropertyChanged]
    public class MainWindowViewModel : ViewModelBase
    {
        readonly IPluginManagerService _plugins;
        readonly MasterDatabaseService _database;
        readonly EnvironmentalCacheService _cache;
        [ImportingConstructor]
        public MainWindowViewModel(MasterDatabaseService database, IPluginManagerService plugins, EnvironmentalCacheService cache)
        {
            _plugins = plugins;
            _plugins.PluginDirectory = PluginDirectory;
            _database = database;
            _database.MasterDatabaseDirectory = _databaseDirectory;
            _cache = cache;
            RootNodes.Clear();
            var scenario = _database.Scenarios.First(); 
            RootNodes.Add(scenario);
            EnvironmentLayers.Add(scenario.Wind);
            EnvironmentLayers.Add(scenario.SoundSpeed);
            EnvironmentLayers.Add(scenario.Bathymetry);
            EnvironmentLayers.Add(scenario.Sediment);
            RootNodes.Add("Environment");
        }
        readonly string _databaseDirectory = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database");
        const string PluginDirectory = @"C:\Projects\ESME Deliverables\Miscellaneous\DavesWPFTester\bin\Debug";

        [Initialize] public ObservableList<object> RootNodes { get; set; }
        [Initialize] public ObservableList<EnvironmentalDataSet> EnvironmentLayers { get; set; }

        #region ViewClosingCommand

        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand
        {
            get
            {
                return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(vcArgs =>
                {
                    Properties.Settings.Default.Save();
                    _database.Context.SaveChanges();
                    _database.Dispose();
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;

        #endregion
    }
}
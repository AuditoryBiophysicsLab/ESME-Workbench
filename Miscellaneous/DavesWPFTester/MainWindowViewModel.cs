using System;
using System.ComponentModel;
using System.Data.Entity;
using System.IO;
using System.Linq;
using Cinch;
using ESME.Locations;
using ESME.Plugins;
using ESME.Scenarios;
using HRC.Aspects;
using MEFedMVVM.ViewModelLocator;
using System.ComponentModel.Composition;

namespace DavesWPFTester
{
    [ExportViewModel("MainWindowViewModel")]
    [NotifyPropertyChanged]
    public class MainWindowViewModel : ViewModelBase
    {
        readonly IPluginManagerService _pluginManager;
        readonly MasterDatabaseService _database;
        readonly EnvironmentalCacheService _cache;
        readonly string _locationRootDirectory = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database");
        const string PluginDirectory = @"C:\Projects\ESME Deliverables\Miscellaneous\DavesWPFTester\bin\Debug";

        [ImportingConstructor]
        public MainWindowViewModel(IPluginManagerService pluginManager, MasterDatabaseService database, EnvironmentalCacheService cache) 
        {
            _pluginManager = pluginManager;
            _pluginManager.PluginDirectory = PluginDirectory;
            _database = database;
            _database.MasterDatabaseDirectory = _locationRootDirectory;
            _cache = cache;
            _database.Context.Scenarios.Load();
            Scenario = _database.Scenarios.First();
        }

        public Scenario Scenario { get; set; }
        //public Location Location { get; set; }
        #region ViewClosingCommand

        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand
        {
            get
            {
                return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(vcArgs =>
                {
                    var ea = (CancelEventArgs)vcArgs.EventArgs;
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
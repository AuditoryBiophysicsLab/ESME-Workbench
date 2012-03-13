using System;
using System.IO;
using System.Linq;
using Cinch;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Locations;
using ESME.Plugins;
using MEFedMVVM.ViewModelLocator;
using System.ComponentModel.Composition;

namespace DavesWPFTester
{
    [ExportViewModel("MainWindowViewModel")]
    public class MainWindowViewModel : ViewModelBase
    {
        readonly IPluginManagerService _pluginManager;
        readonly LocationManagerService _locationManager;
        readonly EnvironmentalDatabaseImportService _importManager;

        [ImportingConstructor]
        public MainWindowViewModel(IPluginManagerService pluginManager, LocationManagerService locationManager, EnvironmentalDatabaseImportService importManager) 
        {
            _pluginManager = pluginManager;
            _pluginManager.PluginDirectory = PluginDirectory;
            _locationManager = locationManager;
            _locationManager.LocationRootDirectory = _locationRootDirectory;
            _importManager = importManager;
        }

        readonly string _locationRootDirectory = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME.LocationService Tests");
        const string PluginDirectory = @"C:\Projects\ESME Deliverables\Miscellaneous\DavesWPFTester\bin\Debug";

        public IPluginManagerService PluginManager
        {
            get { return _pluginManager; }
        }

        #region FillDatabaseCommand
        public SimpleCommand<object, object> FillDatabaseCommand
        {
            get { return _fillDatabase ?? (_fillDatabase = new SimpleCommand<object, object>(delegate { return IsFillDatabaseCommandEnabled; }, delegate { FillDatabaseHandler(); })); }
        }

        SimpleCommand<object, object> _fillDatabase;

        bool IsFillDatabaseCommandEnabled
        {
            get { return true; }
        }

        void FillDatabaseHandler()
        {
#if false
            _locationManager.CreateLocation("Mass Bay", "These are some comments", 44, 41, -69, -72);
            var location = _locationManager.Locations.First(); 
            var soundSpeedCollection = _locationManager.CreateEnvironmentalDataSetCollection(location, new PluginIdentifier
            {
                PluginType = PluginType.EnvironmentalDataSource,
                PluginSubtype = PluginSubtype.SoundSpeed,
                Type = typeof(InstallableNAVOPlugin.GDEM3ForESME).ToString(),
            });
            foreach (var month in NAVOConfiguration.AllMonths) _importManager.BeginImport(_locationManager.CreateEnvironmentalDataSet(soundSpeedCollection, 15, month), new TestProgress { DataType = "SoundSpeed", TimePeriod = month, Resolution = 15 });
            var sedimentCollection = _locationManager.CreateEnvironmentalDataSetCollection(location, new PluginIdentifier
            {
                PluginType = PluginType.EnvironmentalDataSource,
                PluginSubtype = PluginSubtype.Sediment,
                Type = typeof(InstallableNAVOPlugin.BST20ForESME).ToString(),
            });
            _importManager.BeginImport(_locationManager.CreateEnvironmentalDataSet(sedimentCollection, 5f, TimePeriod.Invalid), new TestProgress { DataType = "Sediment", Resolution = 5 });
            var bathymetryCollection = _locationManager.CreateEnvironmentalDataSetCollection(location, new PluginIdentifier
            {
                PluginType = PluginType.EnvironmentalDataSource,
                PluginSubtype = PluginSubtype.Bathymetry,
                Type = typeof(InstallableNAVOPlugin.DBDB54ForESME).ToString(),
            });
            _importManager.BeginImport(_locationManager.CreateEnvironmentalDataSet(bathymetryCollection, 2f, TimePeriod.Invalid), new TestProgress { DataType = "Bathymetry", Resolution = 2 });
            _importManager.BeginImport(_locationManager.CreateEnvironmentalDataSet(bathymetryCollection, 1f, TimePeriod.Invalid), new TestProgress { DataType = "Bathymetry", Resolution = 1 });
            _importManager.BeginImport(_locationManager.CreateEnvironmentalDataSet(bathymetryCollection, 0.5f, TimePeriod.Invalid), new TestProgress { DataType = "Bathymetry", Resolution = 0.5f });
            var windCollection = _locationManager.CreateEnvironmentalDataSetCollection(location, new PluginIdentifier
            {
                PluginType = PluginType.EnvironmentalDataSource,
                PluginSubtype = PluginSubtype.Wind,
                Type = typeof(InstallableNAVOPlugin.SMGC20ForESME).ToString(),
            });
            foreach (var month in NAVOConfiguration.AllMonths)
                _importManager.BeginImport(_locationManager.CreateEnvironmentalDataSet(windCollection, 60, month), new TestProgress { DataType = "Wind", TimePeriod = month, Resolution = 60 });
#endif
        }
        #endregion
    }

    public class TestProgress : IProgress<float>
    {
        public TestProgress() { TimePeriod = TimePeriod.Invalid; }
        public string DataType { get; set; }
        public TimePeriod TimePeriod { get; set; }
        public float Resolution { get; set; }
        public float PercentComplete { get; private set; }

        public void Report(float value)
        {
            Console.WriteLine("{0,10}{1,12}[{2,3} min]: {3,3}%", DataType, TimePeriod == TimePeriod.Invalid ? "" : string.Format("({0})", TimePeriod), Resolution, value);
            PercentComplete = value;
        }
    }
}
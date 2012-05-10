using System;
using System.Data.Entity;
using System.IO;
using System.Linq;
using System.Windows.Media;
using ESME.Database;
using ESME.Locations;
using ESME.Plugins;
using ESME.Scenarios;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;
using MEFedMVVM.ViewModelLocator;
using System.ComponentModel.Composition;

namespace DavesWPFTester
{
    [ExportViewModel("MainWindowViewModel")]
    public class MainWindowViewModel : ViewModelBase
    {
        readonly IPluginManagerService _plugins;
        readonly IMasterDatabaseService _database;
        readonly EnvironmentalCacheService _cache;
        [ImportingConstructor]
        public MainWindowViewModel(IMasterDatabaseService database, IPluginManagerService plugins, EnvironmentalCacheService cache)
        {
            _plugins = plugins;
            _plugins.PluginDirectory = PluginDirectory;
            _database = database;
            _database.MasterDatabaseDirectory = _databaseDirectory;
            _cache = cache;
            RootNodes.Clear();
            _database.Context.Scenarios.Load();
            _database.Context.EnvironmentalDataSets.Load();
            var scenario = _database.Context.Scenarios.Local.First(); 
            RootNodes.Add(scenario);
            RootNodes.Add(new EnvironmentNode(scenario));
            RootNodes.Add(new WorldMapNode());
        }
        readonly string _databaseDirectory = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database");
        const string PluginDirectory = @"C:\Projects\ESME Deliverables\Miscellaneous\DavesWPFTester\bin\Debug";

        [Initialize] public ObservableList<object> RootNodes { get; set; }

        #region ViewClosingCommand

        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand
        {
            get
            {
                return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(vcArgs =>
                {
                    Properties.Settings.Default.Save();
                    _database.Dispose();
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;

        #endregion
    }

    public class EnvironmentNode : ViewModelBase
    {
        public EnvironmentNode(Scenario scenario)
        {
            EnvironmentLayers.Add(scenario.Wind);
            EnvironmentLayers.Add(scenario.SoundSpeed);
            EnvironmentLayers.Add(new BathymetryNode(scenario.Bathymetry));
            EnvironmentLayers.Add(new SedimentNode(scenario.Sediment));
            scenario.PropertyChanged += (s, e) =>
            {
                var sender = (Scenario)s;
                switch (e.PropertyName)
                {
                    case "Wind":
                        EnvironmentLayers[0] = sender.Wind;
                        break;
                    case "SoundSpeed":
                        EnvironmentLayers[1] = sender.SoundSpeed;
                        break;
                    case "Bathymetry":
                        EnvironmentLayers[2] = new BathymetryNode(sender.Bathymetry);
                        break;
                    case "Sediment":
                        EnvironmentLayers[3] = new SedimentNode(sender.Sediment);
                        break;
                }
            };
            IsChecked = true;
        }

        [Initialize] public ObservableList<object> EnvironmentLayers { get; set; }

        public bool? IsChecked { get; set; }
    }

    public class BathymetryNode : ViewModelBase
    {
        public BathymetryNode(EnvironmentalDataSet bathymetry) { Bathymetry = bathymetry; }
        [Initialize(true)]
        public bool IsChecked { get; set; }
        public EnvironmentalDataSet Bathymetry { get; set; }
    }

    public class SedimentNode : ViewModelBase
    {
        public SedimentNode(EnvironmentalDataSet sediment)
        {
            Sediment = sediment;
            SedimentTypes.Add(new SedimentTypeNode { SedimentType = "Ooze" });
            SedimentTypes.Add(new SedimentTypeNode { SedimentType = "Slime" });
            SedimentTypes.Add(new SedimentTypeNode { SedimentType = "Atlantis" });
        }
        [Initialize(true)]
        public bool IsChecked { get; set; }
        public EnvironmentalDataSet Sediment { get; set; }
        [Initialize] 
        public ObservableList<SedimentTypeNode> SedimentTypes { get; set; }
    }

    public class SedimentTypeNode : ViewModelBase
    {
        public static Random Random = new Random();
        public SedimentTypeNode()
        {
            SymbolColor = new DbColor(ColorExtensions.GetRandomNamedColor());
            PointSymbolType = new DbPointSymbolType {PointSymbolTypeAsInt = Random.Next(8)};
        }

        public DbPointSymbolType PointSymbolType { get; set; }
        public Color SymbolColor { get; set; }
        [Initialize(1.0)]
        public double SymbolSize { get; set; }
        [Initialize(true)]
        public bool IsChecked { get; set; }
        public string SedimentType { get; set; }
    }

    public class WorldMapNode : ViewModelBase
    {
        public WorldMapNode()
        {
            LineColor = new DbColor(ColorExtensions.GetRandomNamedColor());
            Features.Add(new WorldMapFeatureNode { FeatureName = "Pan/Zoom" });
            Features.Add(new WorldMapFeatureNode { FeatureName = "Lat/Lon Grid" });
            Features.Add(new WorldMapFeatureNode { FeatureName = "Scale" });
        }

        public Color LineColor { get; set; }
        [Initialize(1.0)]
        public double LineWeight { get; set; }
        [Initialize(true)]
        public bool IsChecked { get; set; }
        [Initialize] 
        public ObservableList<WorldMapFeatureNode> Features { get; set; }
    }

    public class WorldMapFeatureNode : ViewModelBase
    {
        [Initialize(true)]
        public bool IsChecked { get; set; }
        public string FeatureName { get; set; }
    }
}
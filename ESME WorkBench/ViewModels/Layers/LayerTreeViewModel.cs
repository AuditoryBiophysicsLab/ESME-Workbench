using System;
using System.IO;
using System.Reflection;
using System.Windows.Media;
using ESME.Database;
using ESME.Locations;
using ESME.Mapping;
using ESME.Scenarios;
using ESMEWorkbench.Properties;
using ESMEWorkbench.ViewModels.Map;
using HRC.Aspects;
using HRC.Utility;
using HRC.WPF;

namespace ESMEWorkbench.ViewModels.Layers
{
    [NotifyPropertyChanged]
    public class LayerTreeViewModel
    {
        MapViewModel _mapViewModel;
        public MapViewModel MapViewModel
        {
            get { return _mapViewModel; }
            set
            {
                _mapViewModel = value;
                RootNodes.Add(new WorldMapNode(_mapViewModel));
            }
        }

        Scenario _scenario;
        public Scenario Scenario
        {
            get { return _scenario; }
            set
            {
                _scenario = value;
                if (_scenario != null)
                {
                    switch (RootNodes.Count)
                    {
                        case 1:
                            RootNodes.Insert(0, _scenario);
                            RootNodes.Insert(1, new EnvironmentNode(_scenario));
                            break;
                        case 3:
                            RootNodes[0] = _scenario;
                            RootNodes[1] = new EnvironmentNode(_scenario);
                            break;
                        default:
                            throw new ApplicationException(string.Format("LayerTreeViewModel is in an illegal state.  RootNodes.Count = {0}", RootNodes.Count));
                    }
                }
                else
                {
                    switch (RootNodes.Count)
                    {
                        case 1:
                            break;
                        case 3:
                            RootNodes.RemoveAt(0);
                            RootNodes.RemoveAt(0);
                            break;
                        default:
                            throw new ApplicationException(string.Format("LayerTreeViewModel is in an illegal state.  RootNodes.Count = {0}", RootNodes.Count));
                    }
                }
            }
        }

        [Initialize] public ObservableList<object> RootNodes { get; set; }
    }
    [NotifyPropertyChanged]
    public class EnvironmentNode
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
        }

        [Initialize]
        public ObservableList<object> EnvironmentLayers { get; set; }
    }

    [NotifyPropertyChanged]
    public class BathymetryNode
    {
        public BathymetryNode(EnvironmentalDataSet bathymetry) { Bathymetry = bathymetry; }

        public bool IsChecked
        {
            get { return Bathymetry.LayerSettings.IsChecked; }
            set { Bathymetry.LayerSettings.IsChecked = value; }
        }

        public EnvironmentalDataSet Bathymetry { get; set; }
    }

    [NotifyPropertyChanged]
    public class SedimentNode
    {
        public SedimentNode(EnvironmentalDataSet sediment)
        {
            Sediment = sediment;
            SedimentTypes.Add(new SedimentTypeNode { SedimentType = "Ooze" });
            SedimentTypes.Add(new SedimentTypeNode { SedimentType = "Slime" });
            SedimentTypes.Add(new SedimentTypeNode { SedimentType = "Atlantis" });
        }
        public bool IsChecked
        {
            get { return Sediment.LayerSettings.IsChecked; }
            set { Sediment.LayerSettings.IsChecked = value; }
        }
        public EnvironmentalDataSet Sediment { get; set; }
        [Initialize]
        public ObservableList<SedimentTypeNode> SedimentTypes { get; set; }
    }

    [NotifyPropertyChanged]
    public class SedimentTypeNode
    {
        public static Random Random = new Random();
        public SedimentTypeNode()
        {
            SymbolColor = new DbColor(ColorExtensions.GetRandomNamedColor());
            PointSymbolType = new DbPointSymbolType { PointSymbolTypeAsInt = Random.Next(8) };
        }

        public DbPointSymbolType PointSymbolType { get; set; }
        public Color SymbolColor { get; set; }
        [Initialize(1.0)]
        public double SymbolSize { get; set; }
        [Initialize(true)]
        public bool IsChecked { get; set; }
        public string SedimentType { get; set; }
    }

    [NotifyPropertyChanged]
    public class AnalysisPointNode
    {
        
    }

    [NotifyPropertyChanged]
    public class WorldMapNode
    {
        public WorldMapNode(MapViewModel mapViewModel)
        {
            MapViewModel = mapViewModel;
            Features.Add(new WorldMapFeatureNode("Pan/Zoom", () => MapViewModel.IsPanZoomVisible, v => MapViewModel.IsPanZoomVisible = v));
            Features.Add(new WorldMapFeatureNode("Lat/Lon Grid", () => MapViewModel.IsGridVisible, v => MapViewModel.IsGridVisible = v));
            Features.Add(new WorldMapFeatureNode("Scale", () => MapViewModel.IsScaleVisible, v => MapViewModel.IsScaleVisible = v));
            MapViewModel.Add(_worldMapLayer);
            MapViewModel.SetIsVisible(_worldMapLayer, Settings.Default.ShowWorldMap);
        }

        [Affects("LineColor", "LineWeight", "IsChecked", "Features")]
        public MapViewModel MapViewModel { get; set; }

        public Color LineColor
        {
            get { return Settings.Default.WorldMapLineColor; }
            set
            {
                _worldMapLayer.LineColor = value;
                Settings.Default.WorldMapLineColor = value;
            }
        }

        public double LineWeight
        {
            get { return Settings.Default.WorldMapLineWeight; }
            set
            {
                _worldMapLayer.LineWidth = (float)value;
                Settings.Default.WorldMapLineWeight = value;
            }
        }

        public bool IsVisible
        {
            get { return Settings.Default.ShowWorldMap; }
            set
            {
                Settings.Default.ShowWorldMap = value;
                MapViewModel.SetIsVisible(_worldMapLayer, value);
            }
        }

        readonly MapLayerViewModel _worldMapLayer = new ShapefileMapLayer
        {
            LayerType = LayerType.BaseMap,
            AreaColor = Colors.Transparent,
            AreaStyle = MapLayerViewModel.CreateAreaStyle(Settings.Default.WorldMapLineColor, (float)Settings.Default.WorldMapLineWeight, Colors.Transparent),
            CanBeRemoved = false,
            CanBeReordered = true,
            CanChangeAreaColor = true,
            CanChangeLineColor = true,
            ShapefileName = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"Sample GIS Data\Countries02.shp"),
            Name = "World Map",
        };

        [Initialize] public ObservableList<WorldMapFeatureNode> Features { get; set; }
    }

    [NotifyPropertyChanged]
    public class WorldMapFeatureNode
    {
        public WorldMapFeatureNode(string featureName, Func<bool> getter, Action<bool> setter)
        {
            FeatureName = featureName;
            _getter = getter;
            _setter = setter;
        }

        readonly Func<bool> _getter;
        readonly Action<bool> _setter;
        public bool IsChecked
        {
            get { return _getter(); }
            set { _setter(value); }
        }

        public string FeatureName { get; private set; }
    }
}

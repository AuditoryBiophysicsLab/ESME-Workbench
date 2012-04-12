using System;
using System.Windows.Media;
using ESME.Database;
using ESME.Locations;
using ESME.Scenarios;
using HRC.Aspects;
using HRC.Utility;
using HRC.WPF;

namespace ESMEWorkbench.ViewModels.Layers
{
    [NotifyPropertyChanged]
    public class LayerTreeViewModel
    {
        public LayerTreeViewModel()
        {
            RootNodes.Add(new WorldMapNode());
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
            IsChecked = true;
        }

        [Initialize]
        public ObservableList<object> EnvironmentLayers { get; set; }

        public bool? IsChecked { get; set; }
    }

    [NotifyPropertyChanged]
    public class BathymetryNode
    {
        public BathymetryNode(EnvironmentalDataSet bathymetry) { Bathymetry = bathymetry; }
        [Initialize(true)]
        public bool IsChecked { get; set; }
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
        [Initialize(true)]
        public bool IsChecked { get; set; }
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
    public class WorldMapNode
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

    [NotifyPropertyChanged]
    public class WorldMapFeatureNode
    {
        [Initialize(true)]
        public bool IsChecked { get; set; }
        public string FeatureName { get; set; }
    }
}

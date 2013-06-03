using ESME.Locations;
using ESME.Scenarios;
using HRC.Aspects;
using HRC.Utility;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class EnvironmentNode : TreeNodeBase
    {
        public EnvironmentNode(Scenario scenario)
        {
            Location = scenario.Location;
            EnvironmentLayers.Add(new BitmapNode("Wind Speed", scenario.Wind));
            EnvironmentLayers.Add(scenario.SoundSpeed);
            EnvironmentLayers.Add(new BitmapNode("Bathymetry", scenario.Bathymetry));
            EnvironmentLayers.Add(new BitmapNode("Sediment", scenario.Sediment));
            scenario.PropertyChanged += (s, e) =>
            {
                var sender = (Scenario)s;
                switch (e.PropertyName)
                {
                    case "Wind":
                        EnvironmentLayers[0] = new BitmapNode("Wind", sender.Wind);
                        break;
                    case "SoundSpeed":
                        EnvironmentLayers[1] = sender.SoundSpeed;
                        break;
                    case "Bathymetry":
                        EnvironmentLayers[2] = new BitmapNode("Bathymetry", sender.Bathymetry);
                        break;
                    case "Sediment":
                        EnvironmentLayers[3] = new BitmapNode("Sediment", scenario.Sediment);
                        break;
                }
            };
        }

        public Location Location { get; set; }

        [Initialize]
        public ObservableList<object> EnvironmentLayers { get; set; }
    }
}
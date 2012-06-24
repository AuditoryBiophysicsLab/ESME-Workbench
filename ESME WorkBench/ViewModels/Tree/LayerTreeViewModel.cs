using System;
using ESME;
using ESME.Scenarios;
using ESMEWorkbench.ViewModels.Map;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class LayerTreeViewModel : ViewModelBase
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
                switch (RootNodes.Count)
                {
                    case 1:
                        break;
                    case 6:
                        RootNodes.RemoveAt(0);
                        RootNodes.RemoveAt(0);
                        RootNodes.RemoveAt(0);
                        RootNodes.RemoveAt(0);
                        RootNodes.RemoveAt(0);
                        break;
                    default:
                        throw new ApplicationException(string.Format("LayerTreeViewModel is in an illegal state.  RootNodes.Count = {0}", RootNodes.Count));
                }
                _scenario = value;
                if (_scenario == null) return;
                switch (RootNodes.Count)
                {
                    case 1:
                        RootNodes.Insert(0, _scenario);
                        RootNodes.Insert(1, new AnalysisPointsNode(_scenario));
                        RootNodes.Insert(2, new EnvironmentNode(_scenario));
                        RootNodes.Insert(3, new PerimetersNode(_scenario));
                        RootNodes.Insert(4, new SpeciesNode(_scenario));
                        break;
                    case 6:
                        RootNodes[0] = Scenario;
                        RootNodes[1] = new AnalysisPointsNode(_scenario);
                        RootNodes[2] = new EnvironmentNode(_scenario);
                        RootNodes[3] = new PerimetersNode(_scenario);
                        RootNodes[4] = new SpeciesNode(_scenario);
                        break;
                    default:
                        throw new ApplicationException(string.Format("LayerTreeViewModel is in an illegal state.  RootNodes.Count = {0}", RootNodes.Count));
                }
            }
        }

        [Initialize] public ObservableList<IMouseOverAware> RootNodes { get; set; }
    }

    public class TreeNodeBase : ViewModelBase, IMouseOverAware
    {
        public bool IsMouseOver { get; set; }
    }
}



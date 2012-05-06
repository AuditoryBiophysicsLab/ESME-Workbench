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
                _scenario = value;
                Refresh();
            }
        }

        public void Refresh()
        {
            if (Scenario != null)
            {
                switch (RootNodes.Count)
                {
                    case 1:
                        RootNodes.Insert(0, Scenario);
                        RootNodes.Insert(1, new AnalysisPointsNode(Scenario));
                        RootNodes.Insert(2, new EnvironmentNode(Scenario));
                        break;
                    case 4:
                        RootNodes[0] = Scenario;
                        RootNodes[1] = new AnalysisPointsNode(Scenario);
                        RootNodes[2] = new EnvironmentNode(Scenario);
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
                    case 4:
                        RootNodes.RemoveAt(0);
                        RootNodes.RemoveAt(0);
                        RootNodes.RemoveAt(0);
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



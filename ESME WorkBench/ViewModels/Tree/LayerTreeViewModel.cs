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

        [Initialize] public ObservableList<IMouseOverAware> RootNodes { get; set; }
    }

    public class TreeNodeBase : ViewModelBase, IMouseOverAware
    {
        public bool IsMouseOver { get; set; }
    }
}



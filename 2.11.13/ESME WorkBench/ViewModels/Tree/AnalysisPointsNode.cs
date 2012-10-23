using System.Linq;
using ESME;
using ESME.Scenarios;
using HRC.ViewModels;
using HRC.WPF;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class AnalysisPointsNode : ViewModelBase, IMouseOverAware
    {
        public AnalysisPointsNode(Scenario scenario) { Scenario = scenario; }

        public Scenario Scenario { get; set; }

        public bool IsMouseOver
        {
            get { throw new System.NotImplementedException(); }
            set { throw new System.NotImplementedException(); }
        }

        #region DeleteAllCommand
        public SimpleCommand<object, EventToCommandArgs> DeleteAllCommand { get { return _deleteAll ?? (_deleteAll = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeleteAllAnalysisPoints))); } }
        SimpleCommand<object, EventToCommandArgs> _deleteAll;
        #endregion

        #region RecalculateAllCommand
        public SimpleCommand<object, EventToCommandArgs> RecalculateAllCommand { get { return _recalculateAll ?? (_recalculateAll = new SimpleCommand<object, EventToCommandArgs>(o=>MediatorMessage.Send(MediatorMessage.RecalculateAllAnalysisPoints))); } }
        SimpleCommand<object, EventToCommandArgs> _recalculateAll;
        #endregion
        #region Layer Move commands
        #region MoveLayerToFrontCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerToFrontCommand { get { return _moveLayerToFront ?? (_moveLayerToFront = new SimpleCommand<object, EventToCommandArgs>(MoveLayerToFront)); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerToFront;
        void MoveLayerToFront(EventToCommandArgs args)
        {
            foreach (var tl in Scenario.AnalysisPoints.SelectMany(ap => ap.TransmissionLosses)) tl.LayerSettings.MoveLayerToFront();
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion

        #region MoveLayerForwardCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerForwardCommand { get { return _moveLayerForward ?? (_moveLayerForward = new SimpleCommand<object, EventToCommandArgs>(MoveLayerForward)); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerForward;
        void MoveLayerForward(EventToCommandArgs args)
        {
            foreach (var tl in Scenario.AnalysisPoints.SelectMany(ap => ap.TransmissionLosses)) tl.LayerSettings.MoveLayerForward();
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion

        #region MoveLayerBackwardCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerBackwardCommand { get { return _moveLayerBackward ?? (_moveLayerBackward = new SimpleCommand<object, EventToCommandArgs>(MoveLayerBackward)); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerBackward;
        void MoveLayerBackward(EventToCommandArgs args)
        {
            foreach (var tl in Scenario.AnalysisPoints.SelectMany(ap => ap.TransmissionLosses)) tl.LayerSettings.MoveLayerBackward();
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion

        #region MoveLayerToBackCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerToBackCommand { get { return _moveLayerToBack ?? (_moveLayerToBack = new SimpleCommand<object, EventToCommandArgs>(MoveLayerToBack)); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerToBack;
        void MoveLayerToBack(EventToCommandArgs args)
        {
            foreach (var tl in Scenario.AnalysisPoints.SelectMany(ap => ap.TransmissionLosses)) tl.LayerSettings.MoveLayerToBack();
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion
        #endregion    
    }
}
using System;
using System.Collections.Specialized;
using System.Linq;
using ESME;
using ESME.Scenarios;
using HRC;
using HRC.Navigation;
using HRC.ViewModels;
using HRC.WPF;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class AnalysisPointsNode : ViewModelBase, IMouseOverAware
    {
        [UsedImplicitly] PropertyObserver<AnalysisPointsNode> _propertyObserver;
        CollectionObserver _collectionObserver;
        public AnalysisPointsNode(Scenario scenario)
        {
            _propertyObserver = new PropertyObserver<AnalysisPointsNode>(this)
                .RegisterHandler(p => p.Scenario,
                                 () =>
                                 {
                                     if (_collectionObserver != null) _collectionObserver.UnregisterHandler(AnalysisPointCollectionChanged);
                                     else _collectionObserver = new CollectionObserver(Scenario.AnalysisPoints);
                                     _collectionObserver.RegisterHandler(AnalysisPointCollectionChanged);
                                 }
                );
            Scenario = scenario;
            CheckForErrors();
        }

        void AnalysisPointCollectionChanged(INotifyCollectionChanged notifyCollectionChanged, NotifyCollectionChangedEventArgs notifyCollectionChangedEventArgs)
        {
            CheckForErrors();
        }

        public Scenario Scenario { get; set; }

        public bool IsMouseOver
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }
        public bool HasErrors { get; set; }
        public string Errors { get; set; }
        public void CheckForErrors()
        {
            var analysisPointsWithRadialsOutsideLocationBounds = (from analysisPoint in Scenario.AnalysisPoints
                                                                  from transmissionLoss in analysisPoint.TransmissionLosses
                                                                  from radial in transmissionLoss.Radials
                                                                  where !((GeoRect)Scenario.Location.GeoRect).Contains(radial.Segment[1])
                                                                  select analysisPoint).Distinct().ToList();
            if (analysisPointsWithRadialsOutsideLocationBounds.Count > 0)
            {
                Errors = analysisPointsWithRadialsOutsideLocationBounds.Count == 1
                             ? string.Format("An analysis point has one or more radials that extend outside the location boundaries")
                             : string.Format("{0} analysis points have radials that extend outside the location boundaries", analysisPointsWithRadialsOutsideLocationBounds.Count);
                HasErrors = true;
            }
            else
            {
                Errors = string.Empty;
                HasErrors = false;
            }
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
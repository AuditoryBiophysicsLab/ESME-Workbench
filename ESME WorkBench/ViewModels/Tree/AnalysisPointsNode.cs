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
    }
}
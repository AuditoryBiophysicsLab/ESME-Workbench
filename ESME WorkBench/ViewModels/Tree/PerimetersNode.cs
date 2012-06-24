using ESME;
using ESME.Scenarios;
using HRC.ViewModels;
using HRC.WPF;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class PerimetersNode : TreeNodeBase
    {
        public PerimetersNode() {}
        public PerimetersNode(Scenario scenario) { Scenario = scenario; }
        public Scenario Scenario { get; set; }

        #region AddPerimeterCommand
        public SimpleCommand<object, EventToCommandArgs> AddPerimeterCommand { get { return _addPerimeter ?? (_addPerimeter = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.AddPerimeter))); } }
        SimpleCommand<object, EventToCommandArgs> _addPerimeter;
        #endregion
    }
}
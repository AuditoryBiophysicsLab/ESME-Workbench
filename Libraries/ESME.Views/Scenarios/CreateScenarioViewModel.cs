using System.Collections.ObjectModel;
using ESME.Locations;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Scenarios
{
    /// <summary>
    /// To create and show the view as a dialog:
    /// var vm = new CreateScenarioViewModel {...};
    /// var result = _visualizerService.ShowDialog("CreateScenarioView", vm);
    /// if ((!result.HasValue) || (!result.Value)) return;
    /// 
    /// To create and show the view as a window:
    /// var vm = new CreateScenarioViewModel {...};
    /// var window = _visualizerService.ShowWindow("CreateScenarioView", vm);
    /// </summary>
    public class CreateScenarioViewModel : ViewModelBase
    {
        public ObservableCollection<Location> Locations { get; set; }
        public string ScenarioName { get; set; }

        #region OkCommand
        public SimpleCommand<object, EventToCommandArgs> OkCommand { get { return _ok ?? (_ok = new SimpleCommand<object, EventToCommandArgs>(OkHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _ok;

        static void OkHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter;
        }
        #endregion
    }
}

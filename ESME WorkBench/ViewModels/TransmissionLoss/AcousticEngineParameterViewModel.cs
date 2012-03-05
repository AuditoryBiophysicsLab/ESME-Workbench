using System.Collections.ObjectModel;
using System.ComponentModel;
using Cinch;
using ESMEWorkbench.Data;
using ESMEWorkbench.ViewModels.Main;

namespace ESMEWorkbench.ViewModels.TransmissionLoss
{
    class AcousticEngineParameterViewModel : ViewModelBase  
    {
        Experiment _experiment;
        public AcousticEngineParameterViewModel(Experiment experiment)
        {
            _experiment = experiment;
            BellhopParameters = new EditableLabelListViewModel
                                {
                                    LabelWidth = 125,
                                    ValueWidth = 50,
                                    ItemsSource = new ObservableCollection<LabelValuePair>
                                                  {
                                                      new LabelValuePair
                                                      {
                                                          Label = "Range Cell Size (m): ",
                                                          //Value = experiment.BellhopRangeCellSize.ToString(),
                                                      },
                                                      new LabelValuePair
                                                      {
                                                          Label = "Depth Cell Size (m): ",
                                                          //Value = experiment.BellhopDepthCellSize.ToString(),
                                                      },
                                                  }
                                };
        }

        #region public EditableLabelListViewModel BellhopParameters { get; set; }

        public EditableLabelListViewModel BellhopParameters
        {
            get { return _bellhopParameters; }
            set
            {
                if (_bellhopParameters == value) return;
                _bellhopParameters = value;
                NotifyPropertyChanged(BellhopParametersChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BellhopParametersChangedEventArgs = ObservableHelper.CreateArgs<AcousticEngineParameterViewModel>(x => x.BellhopParameters);
        EditableLabelListViewModel _bellhopParameters;

        #endregion

        #region OkCommand

        public SimpleCommand<object, object> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, object>(delegate
                                                                         {
                                                                             //magic validation not-here; here we assume the user values are okay.
                                                                             //todo  scrubscrub
                                                                             //_experiment.BellhopRangeCellSize = Double.Parse(BellhopParameters.ItemsSource[0].Value);
                                                                             //_experiment.BellhopDepthCellSize = Double.Parse(BellhopParameters.ItemsSource[1].Value);
                                                                             CloseActivePopUpCommand.Execute(true);
                                                                         })); }
        }

        SimpleCommand<object, object> _ok;

        #endregion

        #region CancelCommand

        public SimpleCommand<object, object> CancelCommand
        {
            get { return _cancel ?? (_cancel = new SimpleCommand<object, object>(delegate { CloseActivePopUpCommand.Execute(false); })); }
        }

        SimpleCommand<object, object> _cancel;

        #endregion
    }
}

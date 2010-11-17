using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Text;
using Cinch;
using ESMEWorkBench.ViewModels.Main;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    class AcousticEngineParameterViewModel : ViewModelBase  
    {
        public AcousticEngineParameterViewModel()
        {
            BellhopParameters = new EditableLabelListViewModel
                                {
                                    LabelWidth = 125,
                                    ValueWidth = 50,
                                    ItemsSource = new ObservableCollection<LabelValuePair>
                                                  {
                                                      new LabelValuePair
                                                      {
                                                          Label = "Range Cell Size (m): ",
                                                      },
                                                      new LabelValuePair
                                                      {
                                                          Label = "Depth Cell Size (m): ",
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
            get { return _ok ?? (_ok = new SimpleCommand<object, object>(delegate { CloseActivePopUpCommand.Execute(true); })); }
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

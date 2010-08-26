using System.ComponentModel;
using System.ComponentModel.Composition;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMERibbonDemo.ViewModels.Ribbon
{
    [ExportViewModel("RadioButtonDataViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class RadioButtonDataViewModel : ControlDataViewModel
    {
        public bool IsChecked
        {
            get { return _isChecked; }
            set
            {
                if (_isChecked == value) return;
                _isChecked = value;
                NotifyPropertyChanged(IsCheckedChangedEventArgs);
            }
        }
        private static readonly PropertyChangedEventArgs IsCheckedChangedEventArgs = ObservableHelper.CreateArgs<RadioButtonDataViewModel>(x => x.IsChecked);
        private bool _isChecked;
    }
}

using System.ComponentModel;
using System.ComponentModel.Composition;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkbench.ViewModels.Ribbon
{
    [ExportViewModel("CheckBoxDataViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class CheckBoxDataViewModel : ControlDataViewModel
    {
        static readonly PropertyChangedEventArgs IsCheckedChangedEventArgs = ObservableHelper.CreateArgs<CheckBoxDataViewModel>(x => x.IsChecked);
        bool _isChecked;

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
    }
}
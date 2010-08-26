using System.ComponentModel;
using System.ComponentModel.Composition;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMERibbonDemo.ViewModels.Ribbon
{
    [ExportViewModel("SplitButtonDataViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class SplitButtonDataViewModel : MenuButtonDataViewModel
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
        private static readonly PropertyChangedEventArgs IsCheckedChangedEventArgs = ObservableHelper.CreateArgs<SplitButtonDataViewModel>(x => x.IsChecked);
        private bool _isChecked;

        public bool IsCheckable
        {
            get { return _isCheckable; }
            set
            {
                if (_isCheckable == value) return;
                _isCheckable = value;
                NotifyPropertyChanged(IsCheckableChangedEventArgs);
            }
        }
        private static readonly PropertyChangedEventArgs IsCheckableChangedEventArgs = ObservableHelper.CreateArgs<SplitButtonDataViewModel>(x => x.IsCheckable);
        private bool _isCheckable;

        public ButtonDataViewModel DropDownButtonDataViewModel
        {
            get { return _dropDownButtonDataViewModel; }
            set
            {
                if (_dropDownButtonDataViewModel == value) return;
                _dropDownButtonDataViewModel = value;
                NotifyPropertyChanged(DropDownButtonDataViewModelChangedEventArgs);
            }
        }
        private static readonly PropertyChangedEventArgs DropDownButtonDataViewModelChangedEventArgs = ObservableHelper.CreateArgs<SplitButtonDataViewModel>(x => x.DropDownButtonDataViewModel);
        private ButtonDataViewModel _dropDownButtonDataViewModel;
    }
}

using System.ComponentModel;
using System.ComponentModel.Composition;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace OneNavyModel.ViewModels.Ribbon
{
    [ExportViewModel("SplitButtonDataViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class SplitButtonDataViewModel : MenuButtonDataViewModel
    {
        static readonly PropertyChangedEventArgs IsCheckedChangedEventArgs = ObservableHelper.CreateArgs<SplitButtonDataViewModel>(x => x.IsChecked);

        static readonly PropertyChangedEventArgs IsCheckableChangedEventArgs = ObservableHelper.CreateArgs<SplitButtonDataViewModel>(x => x.IsCheckable);

        static readonly PropertyChangedEventArgs DropDownButtonDataViewModelChangedEventArgs = ObservableHelper.CreateArgs<SplitButtonDataViewModel>(x => x.DropDownButtonDataViewModel);
        ButtonDataViewModel _dropDownButtonDataViewModel;
        bool _isCheckable;
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
    }
}
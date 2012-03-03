using System.ComponentModel;
using System.ComponentModel.Composition;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkbench.ViewModels.Ribbon
{
    [ExportViewModel("GroupDataViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class GroupDataViewModel : ControlDataViewModel
    {
        static readonly PropertyChangedEventArgs ControlsChangedEventArgs = ObservableHelper.CreateArgs<GroupDataViewModel>(x => x.Controls);
        ControlList _controls;

        public ControlList Controls
        {
            get { return _controls; }
            set
            {
                if (_controls == value) return;
                _controls = value;
                NotifyPropertyChanged(ControlsChangedEventArgs);
            }
        }
    }
}
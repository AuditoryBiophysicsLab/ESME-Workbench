using System.ComponentModel;
using System.ComponentModel.Composition;
using Cinch;
using MEFedMVVM.ViewModelLocator;
using System.Collections;
using System.Collections.Generic;

namespace ESMERibbonDemo.ViewModels.Ribbon
{
    [ExportViewModel("GroupDataViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class GroupDataViewModel : ControlDataViewModel
    {
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
        private static readonly PropertyChangedEventArgs ControlsChangedEventArgs = ObservableHelper.CreateArgs<GroupDataViewModel>(x => x.Controls);
        private ControlList _controls;
    }
}
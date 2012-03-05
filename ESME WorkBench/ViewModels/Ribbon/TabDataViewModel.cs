using System.ComponentModel;
using System.ComponentModel.Composition;
using Cinch;
using ESME;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkbench.ViewModels.Ribbon
{
    [ExportViewModel("TabDataViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class TabDataViewModel : ViewModelBase, IHaveAName
    {
        static readonly PropertyChangedEventArgs HeaderChangedEventArgs = ObservableHelper.CreateArgs<TabDataViewModel>(x => x.Header);

        static readonly PropertyChangedEventArgs ContextualTabGroupHeaderChangedEventArgs = ObservableHelper.CreateArgs<TabDataViewModel>(x => x.ContextualTabGroupHeader);

        static readonly PropertyChangedEventArgs IsSelectedChangedEventArgs = ObservableHelper.CreateArgs<TabDataViewModel>(x => x.IsSelected);

        static readonly PropertyChangedEventArgs GroupsChangedEventArgs = ObservableHelper.CreateArgs<TabDataViewModel>(x => x.Groups);

        static readonly PropertyChangedEventArgs ControlsChangedEventArgs = ObservableHelper.CreateArgs<TabDataViewModel>(x => x.Controls);
        string _contextualTabGroupHeader;
        ControlList _controls;
        GroupList _groups;
        string _header;
        bool _isSelected;

        public string Header
        {
            get { return _header; }
            set
            {
                if (_header == value) return;
                _header = value;
                NotifyPropertyChanged(HeaderChangedEventArgs);
            }
        }

        public string ContextualTabGroupHeader
        {
            get { return _contextualTabGroupHeader; }
            set
            {
                if (_contextualTabGroupHeader == value) return;
                _contextualTabGroupHeader = value;
                NotifyPropertyChanged(ContextualTabGroupHeaderChangedEventArgs);
            }
        }

        public bool IsSelected
        {
            get { return _isSelected; }
            set
            {
                if (_isSelected == value) return;
                _isSelected = value;
                NotifyPropertyChanged(IsSelectedChangedEventArgs);
            }
        }

        public GroupList Groups
        {
            get { return _groups; }
            set
            {
                if (_groups == value) return;
                _groups = value;
                NotifyPropertyChanged(GroupsChangedEventArgs);
            }
        }

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

        #region IHasName Members

        string IHaveAName.Name
        {
            get { return Header; }
            set { Header = value; }
        }

        #endregion
    }
}
using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMERibbonDemo.ViewModels.Ribbon
{
    [ExportViewModel("TabDataViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class TabDataViewModel : ViewModelBase, IHasName
    {
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
        private static readonly PropertyChangedEventArgs HeaderChangedEventArgs = ObservableHelper.CreateArgs<TabDataViewModel>(x => x.Header);
        private string _header;

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
        private static readonly PropertyChangedEventArgs ContextualTabGroupHeaderChangedEventArgs = ObservableHelper.CreateArgs<TabDataViewModel>(x => x.ContextualTabGroupHeader);
        private string _contextualTabGroupHeader;

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
        private static readonly PropertyChangedEventArgs IsSelectedChangedEventArgs = ObservableHelper.CreateArgs<TabDataViewModel>(x => x.IsSelected);
        private bool _isSelected;

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
        private static readonly PropertyChangedEventArgs GroupsChangedEventArgs = ObservableHelper.CreateArgs<TabDataViewModel>(x => x.Groups);
        private GroupList _groups;

        public ControlList Controls
        {
            get
            {
                return _controls;
            }

            set
            {
                if (_controls == value) return;
                _controls = value;
                NotifyPropertyChanged(ControlsChangedEventArgs);
            }
        }
        private static readonly PropertyChangedEventArgs ControlsChangedEventArgs = ObservableHelper.CreateArgs<TabDataViewModel>(x => x.Controls);
        private ControlList _controls;

        string IHasName.Name
        {
            get { return Header; }
            set { Header = value; }
        }
    }
}

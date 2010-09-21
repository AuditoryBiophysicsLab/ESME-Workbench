using System;
using System.Collections.Generic;
using System.ComponentModel;
using Cinch;

namespace ESMEWorkBench.ViewModels.Main
{
    public class MenuItemViewModel : ViewModelBase
    {
        #region public bool IsCheckable { get; set; }

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

        static readonly PropertyChangedEventArgs IsCheckableChangedEventArgs = ObservableHelper.CreateArgs<MenuItemViewModel>(x => x.IsCheckable);
        bool _isCheckable;

        #endregion

        #region public bool IsChecked { get; set; }

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

        static readonly PropertyChangedEventArgs IsCheckedChangedEventArgs = ObservableHelper.CreateArgs<MenuItemViewModel>(x => x.IsChecked);
        bool _isChecked;

        #endregion

        #region public Object Icon { get; set; }

        public Object Icon
        {
            get { return _icon; }
            set
            {
                if (_icon == value) return;
                _icon = value;
                NotifyPropertyChanged(IconChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IconChangedEventArgs = ObservableHelper.CreateArgs<MenuItemViewModel>(x => x.Icon);
        Object _icon;

        #endregion

        #region public string Header { get; set; }

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

        static readonly PropertyChangedEventArgs HeaderChangedEventArgs = ObservableHelper.CreateArgs<MenuItemViewModel>(x => x.Header);
        string _header;

        #endregion

        #region public IList<MenuViewModel> Children { get; set; }

        public IList<MenuItemViewModel> Children
        {
            get { return _children; }
            set
            {
                if (_children == value) return;
                _children = value;
                NotifyPropertyChanged(ChildrenChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ChildrenChangedEventArgs = ObservableHelper.CreateArgs<MenuItemViewModel>(x => x.Children);
        IList<MenuItemViewModel> _children;

        #endregion

        #region public Object CommandParameter { get; set; }

        public Object CommandParameter
        {
            get { return _commandParameter; }
            set
            {
                if (_commandParameter == value) return;
                _commandParameter = value;
                NotifyPropertyChanged(CommandParameterChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CommandParameterChangedEventArgs = ObservableHelper.CreateArgs<MenuItemViewModel>(x => x.CommandParameter);
        Object _commandParameter;

        #endregion
    
        public SimpleCommand<Object, Object> Command { get; set; }
    }
}

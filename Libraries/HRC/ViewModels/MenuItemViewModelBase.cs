using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Windows;
using Cinch;

namespace HRC.ViewModels
{
    public class MenuItemViewModelBase : ViewModelBase
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

        static readonly PropertyChangedEventArgs IsCheckableChangedEventArgs = ObservableHelper.CreateArgs<MenuItemViewModelBase>(x => x.IsCheckable);
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

        static readonly PropertyChangedEventArgs IsCheckedChangedEventArgs = ObservableHelper.CreateArgs<MenuItemViewModelBase>(x => x.IsChecked);
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

        static readonly PropertyChangedEventArgs IconChangedEventArgs = ObservableHelper.CreateArgs<MenuItemViewModelBase>(x => x.Icon);
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

        static readonly PropertyChangedEventArgs HeaderChangedEventArgs = ObservableHelper.CreateArgs<MenuItemViewModelBase>(x => x.Header);
        string _header;

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

        static readonly PropertyChangedEventArgs CommandParameterChangedEventArgs = ObservableHelper.CreateArgs<MenuItemViewModelBase>(x => x.CommandParameter);
        Object _commandParameter;

        #endregion

        #region public Visibility Visibility { get; set; }

        public Visibility Visibility
        {
            get { return _visibility; }
            set
            {
                if (_visibility == value) return;
                _visibility = value;
                NotifyPropertyChanged(VisibilityChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs VisibilityChangedEventArgs = ObservableHelper.CreateArgs<MenuItemViewModelBase>(x => x.Visibility);
        Visibility _visibility = Visibility.Visible;

        #endregion

        public SimpleCommand<object, object> Command { get; set; }

        #region public IList<MenuItemViewModelBase> Children { get; set; }

        public IList<MenuItemViewModelBase> Children
        {
            get
            {
                if (_children == null)
                {
                    _children = new List<MenuItemViewModelBase>();
                    NotifyPropertyChanged(ChildrenChangedEventArgs);
                }
                return _children;
            }
            set
            {
                if (_children == value) return;
                _children = value;
                NotifyPropertyChanged(ChildrenChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ChildrenChangedEventArgs = ObservableHelper.CreateArgs<MenuItemViewModelBase>(x => x.Children);
        IList<MenuItemViewModelBase> _children;

        #endregion

    }
}         
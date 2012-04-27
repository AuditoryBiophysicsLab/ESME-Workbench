using System;
using System.Collections.Generic;
using System.Windows;
using HRC.Aspects;

namespace HRC.ViewModels
{
    [NotifyPropertyChanged]
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
            }
        }

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
            }
        }

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
            }
        }

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
            }
        }

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
            }
        }

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
            }
        }

        Visibility _visibility = Visibility.Visible;

        #endregion

        public SimpleCommand<object, object> Command { get; set; }

        #region public IList<MenuItemViewModelBase> Children { get; set; }

        public IList<MenuItemViewModelBase> Children
        {
            get { return _children ?? (_children = new List<MenuItemViewModelBase>()); }
            set { _children = value; }
        }

        IList<MenuItemViewModelBase> _children;

        #endregion

    }
}         
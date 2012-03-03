using System.Collections.ObjectModel;
using System.ComponentModel;
using HRC.Utility;

namespace ESMEWorkbench.ViewModels.Ribbon
{
    public class ContextualTabGroupData : PropertyChangedBase
    {
        string _header;
        bool _isVisible;
        ObservableCollection<TabDataViewModel> _tabDataCollection;
        public ContextualTabGroupData() : this(null) {}

        public ContextualTabGroupData(string header)
        {
            Header = header;
        }

        public string Header
        {
            get { return _header; }

            set
            {
                if (_header != value)
                {
                    _header = value;
                    NotifyPropertyChanged(new PropertyChangedEventArgs("Header"));
                }
            }
        }

        public bool IsVisible
        {
            get { return _isVisible; }

            set
            {
                if (_isVisible != value)
                {
                    _isVisible = value;
                    NotifyPropertyChanged(new PropertyChangedEventArgs("IsVisible"));
                }
            }
        }

        public ObservableCollection<TabDataViewModel> TabDataCollection
        {
            get
            {
                if (_tabDataCollection == null)
                    _tabDataCollection = new ObservableCollection<TabDataViewModel>();
                return _tabDataCollection;
            }
        }
    }
}
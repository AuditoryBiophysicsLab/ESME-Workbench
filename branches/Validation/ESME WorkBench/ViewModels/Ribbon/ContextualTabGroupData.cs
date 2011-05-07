using System.Collections.ObjectModel;
using System.ComponentModel;

namespace ESMEWorkBench.ViewModels.Ribbon
{
    public class ContextualTabGroupData : INotifyPropertyChanged
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
                    OnPropertyChanged(new PropertyChangedEventArgs("Header"));
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
                    OnPropertyChanged(new PropertyChangedEventArgs("IsVisible"));
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

        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;

        #endregion

        void OnPropertyChanged(PropertyChangedEventArgs e)
        {
            if (PropertyChanged != null)
                PropertyChanged(this, e);
        }
    }
}
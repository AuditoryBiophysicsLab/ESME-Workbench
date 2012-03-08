using System.Collections.Generic;
using System.Collections.Specialized;
using Cinch;
using HRC.Utility;

namespace ESME.Views.Locations
{
    public class MultipleSelectionsViewModel<T> : ViewModelBase
    {
        #region public IList<float> AvailableSelections { get; set; }

        public IList<T> AvailableSelections
        {
            get { return _availableSelections; }
            set
            {
                if (_availableSelections == value) return;
                _availableSelections = value;
                if (_availableSelections != null)
                {
                    SimpleSelectionViewModels.Clear();
                    foreach (var selection in _availableSelections)
                        SimpleSelectionViewModels.Add(new SimpleSelectionViewModel<T>
                        {
                            Header = string.IsNullOrEmpty(UnitName) ? selection.ToString() : string.Format("{0}{1}", selection, UnitName),
                            IsSelected = false,
                            IsEnabled = true,
                            Value = selection,
                        });
                }
                NotifyPropertyChanged("AvailableSelections");
            }
        }

        IList<T> _availableSelections;

        #endregion

        public string UnitName { get; set; }

        #region public ObservableList<SimpleSelectionViewModel<T>> SimpleSelectionViewModels { get; set; }

        public ObservableList<SimpleSelectionViewModel<T>> SimpleSelectionViewModels
        {
            get { return _simpleSelectionViewModels ?? (_simpleSelectionViewModels = new ObservableList<SimpleSelectionViewModel<T>>()); }
            set
            {
                if (_simpleSelectionViewModels == value) return;
                if (_simpleSelectionViewModels != null) _simpleSelectionViewModels.CollectionChanged -= SimpleSelectionViewModelsCollectionChanged;
                _simpleSelectionViewModels = value;
                if (_simpleSelectionViewModels != null) _simpleSelectionViewModels.CollectionChanged += SimpleSelectionViewModelsCollectionChanged;
                NotifyPropertyChanged("SimpleSelectionViewModels");
            }
        }

        void SimpleSelectionViewModelsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged("SimpleSelectionViewModels"); }
        ObservableList<SimpleSelectionViewModel<T>> _simpleSelectionViewModels;

        #endregion
    }

    public class SimpleSelectionViewModel<T> : ViewModelBase
    {
        #region public string Header { get; set; }

        public string Header
        {
            get { return _header; }
            set
            {
                if (_header == value) return;
                _header = value;
                NotifyPropertyChanged("Header");
            }
        }

        string _header;

        #endregion

        #region public bool IsSelected { get; set; }

        public bool IsSelected
        {
            get { return _isSelected; }
            set
            {
                if (_isSelected == value) return;
                _isSelected = value;
                NotifyPropertyChanged("IsSelected");
            }
        }

        bool _isSelected;

        #endregion

        #region public bool IsEnabled { get; set; }

        public bool IsEnabled
        {
            get { return _isEnabled; }
            set
            {
                if (_isEnabled == value) return;
                _isEnabled = value;
                NotifyPropertyChanged("IsEnabled");
            }
        }

        bool _isEnabled;

        #endregion

        #region public T Value { get; set; }

        public T Value
        {
            get { return _value; }
            set
            {
                _value = value;
                NotifyPropertyChanged("Value");
            }
        }

        T _value;

        #endregion
    }
}

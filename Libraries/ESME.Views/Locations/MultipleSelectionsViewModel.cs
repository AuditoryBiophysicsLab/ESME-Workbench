using System.Collections.Generic;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;

namespace ESME.Views.Locations
{
    [NotifyPropertyChanged]
    public class MultipleSelectionsViewModel<T> : ViewModelBase
    {
        #region public IList<float> AvailableSelections { get; set; }

        public IList<T> AvailableSelections
        {
            get { return _availableSelections; }
            set
            {
                _availableSelections = value;
                if (_availableSelections == null) return;
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
        }

        IList<T> _availableSelections;

        #endregion

        public string UnitName { get; set; }

        [Initialize] public ObservableList<SimpleSelectionViewModel<T>> SimpleSelectionViewModels { get; set; }
    }

    public class SimpleSelectionViewModel<T> : ViewModelBase
    {
        public string Header { get; set; }
        public bool IsSelected { get; set; }
        public bool IsEnabled { get; set; }
        public T Value { get; set; }
    }
}

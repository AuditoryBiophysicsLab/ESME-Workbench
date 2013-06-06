using System.Collections.Generic;
using System.Linq;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;

namespace ESME.Views.Locations
{
    public class MultipleSelectionsViewModel<T> : ViewModelBase
    {
        [Affects("IsSingleItem")]
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
                SelectedItem = SimpleSelectionViewModels.First();
            }
        }
        IList<T> _availableSelections;

        public string UnitName { get; set; }
        public bool IsSingleItem { get { return _availableSelections == null || _availableSelections.Count <= 1; } }
        [Initialize] public ObservableList<SimpleSelectionViewModel<T>> SimpleSelectionViewModels { get; set; }
        public SimpleSelectionViewModel<T> SelectedItem { get; set; }
    }

    public class SimpleSelectionViewModel<T> : ViewModelBase
    {
        public string Header { get; set; }
        public bool IsSelected { get; set; }
        public bool IsEnabled { get; set; }
        public T Value { get; set; }
    }
}

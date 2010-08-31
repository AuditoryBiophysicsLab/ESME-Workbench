using System.Collections.ObjectModel;
using System.ComponentModel;
using System.ComponentModel.Composition;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.Ribbon
{
    [ExportViewModel("GalleryDataViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class GalleryDataViewModel<T> : ControlDataViewModel
    {
        static readonly PropertyChangedEventArgs SelectedItemChangedEventArgs = ObservableHelper.CreateArgs<GalleryDataViewModel<T>>(x => x.SelectedItem);

        static readonly PropertyChangedEventArgs CanUserFilterChangedEventArgs = ObservableHelper.CreateArgs<GalleryDataViewModel<T>>(x => x.CanUserFilter);
        bool _canUserFilter;
        ObservableCollection<GalleryCategoryDataViewModel<T>> _controlDataCollection;
        T _selectedItem;

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Content)]
        public ObservableCollection<GalleryCategoryDataViewModel<T>> CategoryDataCollection
        {
            get { return _controlDataCollection ?? (_controlDataCollection = new ObservableCollection<GalleryCategoryDataViewModel<T>>()); }
        }

        public T SelectedItem
        {
            get { return _selectedItem; }
            set
            {
                if (Equals(value, _selectedItem)) return;
                _selectedItem = value;
                NotifyPropertyChanged(SelectedItemChangedEventArgs);
            }
        }

        public bool CanUserFilter
        {
            get { return _canUserFilter; }
            set
            {
                if (_canUserFilter == value) return;
                _canUserFilter = value;
                NotifyPropertyChanged(CanUserFilterChangedEventArgs);
            }
        }
    }
}
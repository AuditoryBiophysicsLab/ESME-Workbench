using System;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.ComponentModel.Composition;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMERibbonDemo.ViewModels.Ribbon
{
    [ExportViewModel("GalleryDataViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class GalleryDataViewModel<T> : ControlDataViewModel
    {
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Content)]
        public ObservableCollection<GalleryCategoryDataViewModel<T>> CategoryDataCollection
        {
            get
            {
                if (_controlDataCollection == null)
                {
                    _controlDataCollection = new ObservableCollection<GalleryCategoryDataViewModel<T>>();
                }
                return _controlDataCollection;
            }
        }
        private ObservableCollection<GalleryCategoryDataViewModel<T>> _controlDataCollection;

        public T SelectedItem
        {
            get { return _selectedItem; }
            set
            {
                if (Object.Equals(value, _selectedItem)) return;
                _selectedItem = value;
                NotifyPropertyChanged(SelectedItemChangedEventArgs);
            }
        }
        private static readonly PropertyChangedEventArgs SelectedItemChangedEventArgs = ObservableHelper.CreateArgs<GalleryDataViewModel<T>>(x => x.SelectedItem);
        T _selectedItem;

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
        private static readonly PropertyChangedEventArgs CanUserFilterChangedEventArgs = ObservableHelper.CreateArgs<GalleryDataViewModel<T>>(x => x.CanUserFilter);
        private bool _canUserFilter;
    }
}

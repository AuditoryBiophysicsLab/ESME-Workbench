using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using Cinch;
using ESME;
using ESMEWorkBench.ViewModels.Map;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        #region public ObservableCollection<MapLayerViewModel> HomeTabMapLayers { get; set; }

        static readonly PropertyChangedEventArgs HomeTabMapLayersChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.HomeTabMapLayers);
        ObservableCollection<MapLayerViewModel> _homeTabMapLayers = new ObservableCollection<MapLayerViewModel>();

        public ObservableCollection<MapLayerViewModel> HomeTabMapLayers
        {
            get { return _homeTabMapLayers; }
            set
            {
                if (_homeTabMapLayers == value) return;
                if (_homeTabMapLayers != null) _homeTabMapLayers.CollectionChanged -= HomeTabMapLayersCollectionChanged;
                _homeTabMapLayers = value;
                if (_homeTabMapLayers != null) _homeTabMapLayers.CollectionChanged += HomeTabMapLayersCollectionChanged;
                NotifyPropertyChanged(HomeTabMapLayersChangedEventArgs);
            }
        }

        void HomeTabMapLayersCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    if (e.NewItems != null) { }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    break;
                case NotifyCollectionChangedAction.Replace:
                    break;
                case NotifyCollectionChangedAction.Move:
                    break;
                case NotifyCollectionChangedAction.Reset:
                    break;
            }
            NotifyPropertyChanged(HomeTabMapLayersChangedEventArgs);
        }

        #endregion

    }
}

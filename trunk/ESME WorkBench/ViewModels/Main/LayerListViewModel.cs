using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using Cinch;
using ESMEWorkBench.ViewModels.Map;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.Main
{
    [ExportViewModel("LayerListViewModel")]
    public class LayerListViewModel : ViewModelBase
    {
        #region Private fields

        readonly IViewAwareStatus _viewAwareStatusService;
        readonly IMessageBoxService _messageBoxService;

        #endregion

        [ImportingConstructor]
        public LayerListViewModel(IViewAwareStatus viewAwareStatusService, IMessageBoxService messageBoxService)
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nLayerTreeViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
            _viewAwareStatusService = viewAwareStatusService;
            _messageBoxService = messageBoxService;

            _viewAwareStatusService.ViewLoaded += ViewLoaded;

            MapLayers = new ObservableCollection<MapLayerViewModel>();
        }

        #region public ObservableCollection<layerOverlayViewModel> MapLayers { get; set; }

        public ObservableCollection<MapLayerViewModel> MapLayers
        {
            get { return _layerViewModels; }
            set
            {
                if (_layerViewModels == value) return;
                if (_layerViewModels != null) _layerViewModels.CollectionChanged -= ShapeMapLayersCollectionChanged;
                _layerViewModels = value;
                _layerViewModels.CollectionChanged += ShapeMapLayersCollectionChanged;
                NotifyPropertyChanged(MapLayersChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MapLayersChangedEventArgs = ObservableHelper.CreateArgs<LayerListViewModel>(x => x.MapLayers);
        ObservableCollection<MapLayerViewModel> _layerViewModels;

        void ShapeMapLayersCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(MapLayersChangedEventArgs); }

        #endregion

        static void ViewLoaded()
        {
            MediatorMessage.Send(MediatorMessage.LayerListViewModelInitialized);
        }

        [MediatorMessageSink(MediatorMessage.CloseExperiment)]
        void CloseExperiment(bool dummy)
        {
            MapLayers = null;
        }

        [MediatorMessageSink(MediatorMessage.SetLayerCollection)]
        void SetLayerCollection(ObservableCollection<MapLayerViewModel> mapLayers) { MapLayers = mapLayers; }

        [MediatorMessageSink(MediatorMessage.RemoveLayer)]
        void RemoveLayer(MapLayerViewModel layer) { MapLayers.Remove(layer); }

        [MediatorMessageSink(MediatorMessage.LayersReordered)]
        void ReorderLayer(MapLayerViewModel layer) { MapLayers.Move(MapLayers.IndexOf(layer), layer.Index); }
    }
}
using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Windows.Controls;
using Cinch;
using ESME;
using ESME.Mapping;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.Main
{
    [ExportViewModel("LayerListViewModel")]
    public class LayerListViewModel : ViewModelBase
    {
        #region Private fields

        readonly IViewAwareStatus _viewAwareStatus;
        readonly IMessageBoxService _messageBoxService;

        #endregion

        [ImportingConstructor]
        public LayerListViewModel(IViewAwareStatus viewAwareStatus, IMessageBoxService messageBoxService)
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
            _viewAwareStatus = viewAwareStatus;
            _messageBoxService = messageBoxService;

            _viewAwareStatus.ViewLoaded += ViewLoaded;
        }

        #region public MapLayerCollection MapLayers { get; set; }

        public MapLayerCollection MapLayers
        {
            get { return _mapLayers; }
            set
            {
                if (_mapLayers == value) return;
                _mapLayers = value;
                NotifyPropertyChanged(MapLayersChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MapLayersChangedEventArgs = ObservableHelper.CreateArgs<LayerListViewModel>(x => x.MapLayers);
        MapLayerCollection _mapLayers;

        #endregion

        [MediatorMessageSink(MediatorMessage.SetMapLayers)]
        void SetMapLayers(MapLayerCollection mapLayers) { MapLayers = mapLayers; }

        static void ViewLoaded()
        {
            MediatorMessage.Send(MediatorMessage.LayerListViewModelInitialized);
        }

        [MediatorMessageSink(MediatorMessage.RemoveLayer)]
        void RemoveLayer(MapLayerViewModel layer)
        {
            MapLayers.Remove(layer);
            if (layer.LayerType == LayerType.AnalysisPoint) MediatorMessage.Send(MediatorMessage.RemoveAnalysisPoint, layer.AnalysisPoint);
        }

        [MediatorMessageSink(MediatorMessage.LayersReordered)]
        void ReorderLayer(MapLayerViewModel layer)
        {
            var layerIndex = MapLayers.IndexOf(layer);
            if (layerIndex == -1) return;
            MapLayers.Move(layerIndex, layer.Index); 
        }

        [MediatorMessageSink(MediatorMessage.EnableGUI)]
        void EnableGUI(bool enable)
        {
            ((UserControl)_viewAwareStatus.View).IsEnabled = enable;
        }
    }
}
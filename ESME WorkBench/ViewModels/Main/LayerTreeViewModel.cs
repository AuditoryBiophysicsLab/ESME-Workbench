using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using Cinch;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.Map;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.Main
{
    [ExportViewModel("LayerTreeViewModel")]
    public class LayerTreeViewModel : ViewModelBase
    {
        #region Private fields

        readonly IViewAwareStatus _viewAwareStatusService;
        readonly IMessageBoxService _messageBoxService;

        #endregion

        [ImportingConstructor]
        public LayerTreeViewModel(IViewAwareStatus viewAwareStatusService, IMessageBoxService messageBoxService)
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

            MoveLayerToBackCommand = new SimpleCommand<object, object>(CanMoveLayerBackCommand, ExecuteMoveLayerToBackCommand);
            MoveLayerBackCommand = new SimpleCommand<object, object>(CanMoveLayerBackCommand, ExecuteMoveLayerBackwardCommand);
            MoveLayerForwardCommand = new SimpleCommand<object, object>(CanMoveLayerForwardCommand, ExecuteMoveLayerForwardCommand);
            MoveLayerToFrontCommand = new SimpleCommand<object, object>(CanMoveLayerForwardCommand, ExecuteMoveLayerToFrontCommand);
            Layers = new ObservableCollection<LayerViewModel>();
        }

        #region public ObservableCollection<LayerViewModel> Layers { get; set; }

        public ObservableCollection<LayerViewModel> Layers
        {
            get { return _layers; }
            set
            {
                if (_layers == value) return;
                _layers = value;
                _layers.CollectionChanged += ShapeLayersCollectionChanged;
                NotifyPropertyChanged(LayersChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LayersChangedEventArgs = ObservableHelper.CreateArgs<LayerTreeViewModel>(x => x.Layers);
        ObservableCollection<LayerViewModel> _layers;

        void ShapeLayersCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(LayersChangedEventArgs); }

        #endregion

        public SimpleCommand<Object, Object> MoveLayerToBackCommand { get; private set; }
        public SimpleCommand<Object, Object> MoveLayerBackCommand { get; private set; }
        public SimpleCommand<Object, Object> MoveLayerForwardCommand { get; private set; }
        public SimpleCommand<Object, Object> MoveLayerToFrontCommand { get; private set; }

        static void ViewLoaded()
        {
            Mediator.Instance.NotifyColleagues("LayerTreeViewModelInitializedMessage", true);
        }

        void ExecuteMoveLayerForwardCommand(Object args)
        {
            var sourceLayer = (LayerViewModel) args;
            if (sourceLayer == null) return;
            var layerIndex = Layers.IndexOf(sourceLayer);
            var targetLayer = Layers[layerIndex - 1];
            //_mapViewModel.Overlays.MoveTo(layer.Overlay, layerIndex + 2);
            Layers.Move(layerIndex, layerIndex + 1);
            Mediator.Instance.NotifyColleagues("ReorderLayersInMapViewMessage", new MapLayerReorderDescriptor
                                                                                {
                                                                                    MapLayerReorderCommand = MapLayerReorderCommand.MoveForward,
                                                                                    SourceLayer = sourceLayer,
                                                                                    TargetLayer = targetLayer,
                                                                                });
            Mediator.Instance.NotifyColleagues("RefreshMapViewMessage", true);
        }

        void ExecuteMoveLayerBackwardCommand(Object args)
        {
            var sourceLayer = (LayerViewModel) args;
            if (sourceLayer == null) return;
            var layerIndex = Layers.IndexOf(sourceLayer);
            var targetLayer = Layers[layerIndex - 1];
            //_mapViewModel.Overlays.MoveTo(layer.Overlay, layerIndex);
            Layers.Move(layerIndex, layerIndex - 1);
            Mediator.Instance.NotifyColleagues("ReorderLayersInMapViewMessage", new MapLayerReorderDescriptor
                                                                                {
                                                                                    MapLayerReorderCommand = MapLayerReorderCommand.MoveBackward,
                                                                                    SourceLayer = sourceLayer,
                                                                                    TargetLayer = targetLayer,
                                                                                });
            Mediator.Instance.NotifyColleagues("RefreshMapViewMessage", true);
        }

        void ExecuteMoveLayerToBackCommand(Object args)
        {
            var sourceLayer = (LayerViewModel) args;
            if (sourceLayer == null) return;
            var layerIndex = Layers.IndexOf(sourceLayer);
            //_mapViewModel.Overlays.MoveTo(layer.Overlay, 1);
            Layers.Move(layerIndex, 0);
            Mediator.Instance.NotifyColleagues("ReorderLayersInMapViewMessage", new MapLayerReorderDescriptor
                                                                                {
                                                                                    MapLayerReorderCommand = MapLayerReorderCommand.MoveToBack,
                                                                                    SourceLayer = sourceLayer,
                                                                                    TargetLayer = null,
                                                                                });
            Mediator.Instance.NotifyColleagues("RefreshMapViewMessage", true);
        }

        void ExecuteMoveLayerToFrontCommand(Object args)
        {
            var sourceLayer = (LayerViewModel) args;
            if (sourceLayer == null) return;
            var layerIndex = Layers.IndexOf(sourceLayer);
            //_mapViewModel.Overlays.MoveTo(layer.Overlay, _mapViewModel.Overlays.Count - 1);
            Layers.Move(layerIndex, Layers.Count - 1);
            Mediator.Instance.NotifyColleagues("ReorderLayersInMapViewMessage", new MapLayerReorderDescriptor
                                                                                {
                                                                                    MapLayerReorderCommand = MapLayerReorderCommand.MoveToFront,
                                                                                    SourceLayer = sourceLayer,
                                                                                    TargetLayer = null,
                                                                                });
            Mediator.Instance.NotifyColleagues("RefreshMapViewMessage", true);
        }

        bool CanMoveLayerBackCommand(Object args)
        {
            var layer = (LayerViewModel) args;
            if (layer == null) return false;
            var layerIndex = Layers.IndexOf(layer);
            return (layerIndex > 1) && (Layers.Count > 1);
        }

        bool CanMoveLayerForwardCommand(Object args)
        {
            var layer = (LayerViewModel) args;
            if (layer == null) return false;
            var layerIndex = Layers.IndexOf(layer);
            return ((layerIndex < (Layers.Count - 1)) && (Layers.Count > 1));
        }

        [MediatorMessageSink("AddLayerToTreeViewMessage")]
        void AddLayer(LayerViewModel layer)
        {
            layer.LayerTreeTreeViewModel = this;
            Layers.Add(layer);
        }
    }
}
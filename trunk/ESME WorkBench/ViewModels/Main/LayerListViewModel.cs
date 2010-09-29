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

            MoveLayerToTopCommand = new SimpleCommand<LayerViewModel, LayerViewModel>(CanLayerMoveUp, ExecuteMoveLayerToTopCommand);
            MoveLayerUpCommand = new SimpleCommand<LayerViewModel, LayerViewModel>(CanLayerMoveUp, ExecuteMoveLayerUpCommand);
            MoveLayerDownCommand = new SimpleCommand<LayerViewModel, LayerViewModel>(CanLayerMoveDown, ExecuteMoveLayerDownCommand);
            MoveLayerToBottomCommand = new SimpleCommand<LayerViewModel, LayerViewModel>(CanLayerMoveDown, ExecuteMoveLayerToBottomCommand);
            LayerViewModels = new ObservableCollection<LayerViewModel>();
        }

        #region public ObservableCollection<layerOverlayViewModel> LayerViewModels { get; set; }

        public ObservableCollection<LayerViewModel> LayerViewModels
        {
            get { return _layerViewModels; }
            set
            {
                if (_layerViewModels == value) return;
                _layerViewModels = value;
                _layerViewModels.CollectionChanged += ShapeLayerViewModelsCollectionChanged;
                NotifyPropertyChanged(LayerViewModelsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LayerViewModelsChangedEventArgs = ObservableHelper.CreateArgs<LayerListViewModel>(x => x.LayerViewModels);
        ObservableCollection<LayerViewModel> _layerViewModels;

        void ShapeLayerViewModelsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(LayerViewModelsChangedEventArgs); }

        #endregion

        public SimpleCommand<LayerViewModel, LayerViewModel> MoveLayerToTopCommand { get; private set; }
        public SimpleCommand<LayerViewModel, LayerViewModel> MoveLayerUpCommand { get; private set; }
        public SimpleCommand<LayerViewModel, LayerViewModel> MoveLayerDownCommand { get; private set; }
        public SimpleCommand<LayerViewModel, LayerViewModel> MoveLayerToBottomCommand { get; private set; }

        static void ViewLoaded()
        {
            MediatorMessage.Send(MediatorMessage.LayerListViewModelInitialized);
        }

        static void ExecuteMoveLayerUpCommand(LayerViewModel sourceLayer)
        {
            if (sourceLayer == null) return;
            MediatorMessage.Send(MediatorMessage.MoveLayerUp, sourceLayer.MapLayer);
            MediatorMessage.Send(MediatorMessage.RefreshMap);
        }

        static void ExecuteMoveLayerDownCommand(LayerViewModel sourceLayer)
        {
            if (sourceLayer == null) return;
            MediatorMessage.Send(MediatorMessage.MoveLayerDown, sourceLayer.MapLayer);
            MediatorMessage.Send(MediatorMessage.RefreshMap);
        }

        static void ExecuteMoveLayerToBottomCommand(LayerViewModel sourceLayer)
        {
            if (sourceLayer == null) return;
            MediatorMessage.Send(MediatorMessage.MoveLayerToBottom, sourceLayer.MapLayer);
            MediatorMessage.Send(MediatorMessage.RefreshMap);
        }

        static void ExecuteMoveLayerToTopCommand(LayerViewModel sourceLayer)
        {
            if (sourceLayer == null) return;
            MediatorMessage.Send(MediatorMessage.MoveLayerToTop, sourceLayer.MapLayer);
            MediatorMessage.Send(MediatorMessage.RefreshMap);
        }

        bool CanLayerMoveUp(LayerViewModel sourceLayer)
        {
            if (sourceLayer == null) return false;
            var layerIndex = LayerViewModels.IndexOf(sourceLayer);
            return (layerIndex > 0) && (LayerViewModels.Count > 1);
        }

        bool CanLayerMoveDown(LayerViewModel sourceLayer)
        {
            if (sourceLayer == null) return false;
            var layerIndex = LayerViewModels.IndexOf(sourceLayer);
            return ((layerIndex < (LayerViewModels.Count - 1)) && (LayerViewModels.Count > 1));
        }

        [MediatorMessageSink(MediatorMessage.AddListLayer)]
        void AddListLayer(LayerViewModel layer) { layer.LayerListListViewModel = this; LayerViewModels.Add(layer); }

        [MediatorMessageSink(MediatorMessage.RemoveLayer)]
        void RemoveLayer(MapLayer layer) { LayerViewModels.Remove(layer.LayerViewModel); }
    }
}
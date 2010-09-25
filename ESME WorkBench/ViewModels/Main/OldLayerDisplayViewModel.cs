using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using Cinch;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.Map;

namespace ESMEWorkBench.ViewModels.Main
{
    public class OldLayerDisplayViewModel : ViewModelBase
    {
        static readonly PropertyChangedEventArgs LayersChangedEventArgs = ObservableHelper.CreateArgs<OldLayerDisplayViewModel>(x => x.Layers);
        ObservableCollection<LayerViewModel> _layers;
        readonly MapViewModel _mapViewModel;

        public OldLayerDisplayViewModel(MapViewModel mapViewModel)
        {
            MoveLayerToBackCommand = new SimpleCommand<object, object>(CanMoveLayerBackCommand, ExecuteMoveLayerToBackCommand);
            MoveLayerBackCommand = new SimpleCommand<object, object>(CanMoveLayerBackCommand, ExecuteMoveLayerBackwardCommand);
            MoveLayerForwardCommand = new SimpleCommand<object, object>(CanMoveLayerForwardCommand, ExecuteMoveLayerForwardCommand);
            MoveLayerToFrontCommand = new SimpleCommand<object, object>(CanMoveLayerForwardCommand, ExecuteMoveLayerToFrontCommand);
            Layers = new ObservableCollection<LayerViewModel>();
            _mapViewModel = mapViewModel;
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

        public SimpleCommand<Object, Object> MoveLayerToBackCommand { get; private set; }
        public SimpleCommand<Object, Object> MoveLayerBackCommand { get; private set; }
        public SimpleCommand<Object, Object> MoveLayerForwardCommand { get; private set; }
        public SimpleCommand<Object, Object> MoveLayerToFrontCommand { get; private set; }

        void ShapeLayersCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            NotifyPropertyChanged(LayersChangedEventArgs);
        }

        #endregion

        void ExecuteMoveLayerForwardCommand(Object args)
        {
            var layer = (LayerViewModel)args;
            if (layer == null) return;
            var layerIndex = Layers.IndexOf(layer);
            _mapViewModel.Overlays.MoveTo(layer.Overlay, layerIndex + 2);
            Layers.Move(layerIndex, layerIndex + 1);
            Mediator.Instance.NotifyColleagues("RefreshMapViewMessage");
        }

        void ExecuteMoveLayerBackwardCommand(Object args)
        {
            var layer = (LayerViewModel)args;
            if (layer == null) return;
            var layerIndex = Layers.IndexOf(layer);
            _mapViewModel.Overlays.MoveTo(layer.Overlay, layerIndex);
            Layers.Move(layerIndex, layerIndex - 1);
            Mediator.Instance.NotifyColleagues("RefreshMapViewMessage");
        }

        void ExecuteMoveLayerToBackCommand(Object args)
        {
            var layer = (LayerViewModel)args;
            if (layer == null) return;
            var layerIndex = Layers.IndexOf(layer);
            _mapViewModel.Overlays.MoveTo(layer.Overlay, 1);
            Layers.Move(layerIndex, 0);
            Mediator.Instance.NotifyColleagues("RefreshMapViewMessage");
        }

        void ExecuteMoveLayerToFrontCommand(Object args)
        {
            var layer = (LayerViewModel)args;
            if (layer == null) return;
            var layerIndex = Layers.IndexOf(layer);
            _mapViewModel.Overlays.MoveTo(layer.Overlay, _mapViewModel.Overlays.Count - 1);
            Layers.Move(layerIndex, Layers.Count - 1);
            Mediator.Instance.NotifyColleagues("RefreshMapViewMessage");
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
            var layer = (LayerViewModel)args;
            if (layer == null) return false;
            var layerIndex = Layers.IndexOf(layer);
            return ((layerIndex < (Layers.Count - 1)) && (Layers.Count > 1));
        }
    }
}
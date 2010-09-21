using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using Cinch;
using ESMEWorkBench.ViewModels.Layers;

namespace ESMEWorkBench.ViewModels.Main
{
    public class LayerDisplayViewModel : ViewModelBase
    {
        static readonly PropertyChangedEventArgs LayersChangedEventArgs = ObservableHelper.CreateArgs<LayerDisplayViewModel>(x => x.Layers);
        ObservableCollection<LayerViewModel> _layers;
        readonly MapViewModel _mapViewModel;

        public LayerDisplayViewModel(MapViewModel mapViewModel)
        {
            MoveLayerToBackCommand = new SimpleCommand<object, object>(CanMoveLayerUpCommand, ExecuteMoveLayerToTopCommand);
            MoveLayerBackCommand = new SimpleCommand<object, object>(CanMoveLayerUpCommand, ExecuteMoveLayerUpCommand);
            MoveLayerForwardCommand = new SimpleCommand<object, object>(CanMoveLayerDownCommand, ExecuteMoveLayerDownCommand);
            MoveLayerToFrontCommand = new SimpleCommand<object, object>(CanMoveLayerDownCommand, ExecuteMoveLayerToBottomCommand);
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

        void ExecuteMoveLayerUpCommand(Object args)
        {
            var layer = (LayerViewModel)args;
            if (layer == null) return;
            var layerIndex = Layers.IndexOf(layer);
            _mapViewModel.Overlays.MoveTo(layerIndex + 1, layerIndex);
            Layers.Move(layerIndex, layerIndex - 1);
            _mapViewModel.Refresh();
        }

        void ExecuteMoveLayerDownCommand(Object args)
        {
            var layer = (LayerViewModel)args;
            if (layer == null) return;
            var layerIndex = Layers.IndexOf(layer);
            _mapViewModel.Overlays.MoveTo(layerIndex + 1, layerIndex + 2);
            Layers.Move(layerIndex, layerIndex + 1);
            _mapViewModel.Refresh();
        }

        void ExecuteMoveLayerToTopCommand(Object args)
        {
            var layer = (LayerViewModel)args;
            if (layer == null) return;
            var layerIndex = Layers.IndexOf(layer);
            _mapViewModel.Overlays.MoveTo(layerIndex, 1);
            Layers.Move(layerIndex, 0);
            _mapViewModel.Refresh();
        }

        void ExecuteMoveLayerToBottomCommand(Object args)
        {
            var layer = (LayerViewModel)args;
            if (layer == null) return;
            var layerIndex = Layers.IndexOf(layer);
            _mapViewModel.Overlays.MoveTo(layerIndex, _mapViewModel.Overlays.Count - 1);
            Layers.Move(layerIndex, Layers.Count - 1);
            _mapViewModel.Refresh();
        }

        bool CanMoveLayerUpCommand(Object args)
        {
            var layer = (LayerViewModel) args;
            if (layer == null) return false;
            var layerIndex = Layers.IndexOf(layer);
            return (layerIndex > 0) && (Layers.Count > 1);
        }

        bool CanMoveLayerDownCommand(Object args)
        {
            var layer = (LayerViewModel)args;
            if (layer == null) return false;
            var layerIndex = Layers.IndexOf(layer);
            return ((layerIndex < (Layers.Count - 1)) && (Layers.Count > 1));
        }
    }
}
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Linq;
using Cinch;
using ESMEWorkBench.ViewModels.Layers;
using MEFedMVVM.ViewModelLocator;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Main
{
    [ExportViewModel("LayerDisplayViewModel", false)]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class LayerDisplayViewModel : ViewModelBase
    {
        static readonly PropertyChangedEventArgs LayersChangedEventArgs = ObservableHelper.CreateArgs<LayerDisplayViewModel>(x => x.Layers);
        ObservableCollection<LayerViewModel> _layers;
        LayerViewModel _selectedLayer;
        int _selectedIndex;
        readonly MapViewModel _mapViewModel;

        public LayerDisplayViewModel(MapViewModel mapViewModel)
        {
            MoveLayerToTopCommand = new SimpleCommand<object, object>(CanMoveLayerUpCommand, ExecuteMoveLayerToTopCommand);
            MoveLayerUpCommand = new SimpleCommand<object, object>(CanMoveLayerUpCommand, ExecuteMoveLayerUpCommand);
            MoveLayerDownCommand = new SimpleCommand<object, object>(CanMoveLayerDownCommand, ExecuteMoveLayerDownCommand);
            MoveLayerToBottomCommand = new SimpleCommand<object, object>(CanMoveLayerDownCommand, ExecuteMoveLayerToBottomCommand);
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

        public void Clear()
        {
            foreach (var layer in Layers)
            {
                layer.Remove();
                layer.PropertyChanged -= Layer_PropertyChanged;
            }
            Layers.Clear();
        }

        public SimpleCommand<Object, Object> MoveLayerToTopCommand { get; private set; }
        public SimpleCommand<Object, Object> MoveLayerUpCommand { get; private set; }
        public SimpleCommand<Object, Object> MoveLayerDownCommand { get; private set; }
        public SimpleCommand<Object, Object> MoveLayerToBottomCommand { get; private set; }

        void ShapeLayersCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Move:
                case NotifyCollectionChangedAction.Reset:
                    return;
                case NotifyCollectionChangedAction.Add:
                case NotifyCollectionChangedAction.Remove:
                case NotifyCollectionChangedAction.Replace:
                    break;
            }
            if (e.NewItems != null)
                foreach (var newLayer in e.NewItems.Cast<LayerViewModel>())
                {
                    newLayer.PropertyChanged += Layer_PropertyChanged;
                    _mapViewModel.Refresh();
                }
            if (e.OldItems != null)
                foreach (var oldLayer in e.OldItems.Cast<LayerViewModel>())
                {
                    oldLayer.PropertyChanged -= Layer_PropertyChanged;
                    oldLayer.Remove();
                    _mapViewModel.Refresh();
                }
            NotifyPropertyChanged(LayersChangedEventArgs);
        }

        void Layer_PropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            var src = (LayerViewModel)sender;
            switch (e.PropertyName)
            {
                case "IsSelected":
                    _selectedLayer = null;
                    if (src.IsSelected)
                        _selectedLayer = src;
                    break;
                default:
                    return;
            }
        }
        #endregion

        void ExecuteMoveLayerUpCommand(Object args)
        {
            _mapViewModel.Overlays.MoveTo(_selectedIndex + 1, _selectedIndex);
            Layers.Move(_selectedIndex, _selectedIndex - 1);
            _mapViewModel.Refresh();
        }

        void ExecuteMoveLayerDownCommand(Object args)
        {
            _mapViewModel.Overlays.MoveTo(_selectedIndex + 1, _selectedIndex + 2);
            Layers.Move(_selectedIndex, _selectedIndex + 1);
            _mapViewModel.Refresh();
        }

        void ExecuteMoveLayerToTopCommand(Object args)
        {
            _mapViewModel.Overlays.MoveTo(_selectedIndex + 1, 1);
            Layers.Move(_selectedIndex, 0);
            _mapViewModel.Refresh();
        }

        void ExecuteMoveLayerToBottomCommand(Object args)
        {
            _mapViewModel.Overlays.MoveTo(_selectedIndex + 1, _mapViewModel.Overlays.Count - 1);
            Layers.Move(_selectedIndex, Layers.Count - 1);
            _mapViewModel.Refresh();
        }

        bool CanMoveLayerUpCommand(Object args)
        {
            if (_selectedLayer == null)
                return false;
            _selectedIndex = Layers.IndexOf(_selectedLayer);
            return _selectedIndex > 0;
        }

        bool CanMoveLayerDownCommand(Object args)
        {
            if (_selectedLayer == null)
                return false;
            _selectedIndex = Layers.IndexOf(_selectedLayer);
            return _selectedIndex < (Layers.Count - 1);
        }
    }
}
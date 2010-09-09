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
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Main
{
    [ExportViewModel("LayerDisplayViewModel", false)]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class LayerDisplayViewModel : ViewModelBase
    {
        static readonly PropertyChangedEventArgs LayersChangedEventArgs = ObservableHelper.CreateArgs<LayerDisplayViewModel>(x => x.Layers);
        ObservableCollection<LayerViewModel> _layers;
        LayerViewModel _selectedLayer;
        readonly WpfMap _wpfMap;

        public LayerDisplayViewModel(WpfMap wpfMap)
        {
            _wpfMap = wpfMap;
            MoveLayerToTopCommand = new SimpleCommand<object, object>(CanMoveLayerCommand, ExecuteMoveLayerToTopCommand);
            MoveLayerUpCommand = new SimpleCommand<object, object>(CanMoveLayerCommand, ExecuteMoveLayerUpCommand);
            MoveLayerDownCommand = new SimpleCommand<object, object>(CanMoveLayerCommand, ExecuteMoveLayerDownCommand);
            MoveLayerToBottomCommand = new SimpleCommand<object, object>(CanMoveLayerCommand, ExecuteMoveLayerToBottomCommand);
            Layers = new ObservableCollection<LayerViewModel>();
            SortedLayers = new List<LayerViewModel>();
        }

        #region public List<LayerViewModel> SortedLayers { get; set; }
        public List<LayerViewModel> SortedLayers
        {
            get { return _sortedLayers; }
            set
            {
                if (_sortedLayers == value) return;
                _sortedLayers = value;
                NotifyPropertyChanged(SortedLayersChangedEventArgs);
            }
        }
        private List<LayerViewModel> _sortedLayers;
        static readonly PropertyChangedEventArgs SortedLayersChangedEventArgs = ObservableHelper.CreateArgs<LayerDisplayViewModel>(x => x.SortedLayers);
        #endregion

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
            SortedLayers.Clear();
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
                    SortedLayers.Add(newLayer);
                    SortedLayers.Sort();
                    NotifyPropertyChanged(SortedLayersChangedEventArgs);
                }
            if (e.OldItems != null)
                foreach (var oldLayer in e.OldItems.Cast<LayerViewModel>())
                {
                    oldLayer.PropertyChanged -= Layer_PropertyChanged;
                    oldLayer.Remove();
                    SortedLayers.Remove(oldLayer);
                    SortedLayers.Sort();
                    NotifyPropertyChanged(SortedLayersChangedEventArgs);
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
                case "Index":
                    Layers.Move(src.Index, src.NewIndex);
                    SortedLayers.Sort();
                    NotifyPropertyChanged(SortedLayersChangedEventArgs);
                    break;
                default:
                    return;
            }
        }
        #endregion
        //Function for getting the extent based on a collection of layers.
        //It gets the overall extent of all the layers.
        private RectangleShape GetFullExtent(IEnumerable<Layer> layers)
        {
            var rectangleShapes = new Collection<BaseShape>();

            foreach (var layer in layers)
            {
                layer.Open();
                if (layer.HasBoundingBox) rectangleShapes.Add(layer.GetBoundingBox());
            }
            return ExtentHelper.GetBoundingBoxOfItems(rectangleShapes);
        }


        void ExecuteMoveLayerUpCommand(Object args)
        {
            _selectedLayer.MoveUp();
            _wpfMap.Refresh();
        }

        void ExecuteMoveLayerDownCommand(Object args)
        {
            _selectedLayer.MoveDown();
            _wpfMap.Refresh();
        }

        void ExecuteMoveLayerToTopCommand(Object args)
        {
            _selectedLayer.MoveToTop();
            _wpfMap.Refresh();
        }

        void ExecuteMoveLayerToBottomCommand(Object args)
        {
            _selectedLayer.MoveToBottom();
            _wpfMap.Refresh();
        }

        bool CanMoveLayerCommand(Object args)
        {
            return _selectedLayer != null;
        }
    }
}
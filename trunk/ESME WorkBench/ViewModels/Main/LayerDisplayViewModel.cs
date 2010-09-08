using System;
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
            MoveLayerUpCommand = new SimpleCommand<object, object>(CanMoveLayerCommand, ExecuteMoveLayerUpCommand);
            MoveLayerDownCommand = new SimpleCommand<object, object>(CanMoveLayerCommand, ExecuteMoveLayerDownCommand);
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

        //Function for getting the extent based on a collection of layers.
        //It gets the overall extent of all the layers.
        private RectangleShape GetFullExtent(GeoCollection<Layer> Layers)
        {
            Collection<BaseShape> rectangleShapes = new Collection<BaseShape>();

            foreach (Layer layer in Layers)
            {
                layer.Open();
                if (layer.HasBoundingBox == true) rectangleShapes.Add(layer.GetBoundingBox());
            }
            return ExtentHelper.GetBoundingBoxOfItems(rectangleShapes);
        }

        public void SetViewFullExtent()
        {
            _wpfMap.CurrentExtent = ExtentHelper.GetDrawingExtent(new RectangleShape(-180, 90, 180, -90), (float)_wpfMap.Width, (float)_wpfMap.Height);
        }

        public SimpleCommand<Object, Object> MoveLayerUpCommand { get; private set; }
        public SimpleCommand<Object, Object> MoveLayerDownCommand { get; private set; }

        void ShapeLayersCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (e.NewItems != null)
                foreach (var newLayer in e.NewItems.Cast<LayerViewModel>())
                    newLayer.PropertyChanged += Layer_PropertyChanged;
            if (e.OldItems != null)
                foreach (var oldLayer in e.OldItems.Cast<LayerViewModel>())
                    oldLayer.PropertyChanged -= Layer_PropertyChanged;
            NotifyPropertyChanged(LayersChangedEventArgs);
        }

        void Layer_PropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            if (e.PropertyName != "IsSelected") return;
            var src = (LayerViewModel) sender;
            if (src.IsSelected)
                _selectedLayer = src;
        }
        #endregion

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

        bool CanMoveLayerCommand(Object args)
        {
            return _selectedLayer != null;
        }
    }
}
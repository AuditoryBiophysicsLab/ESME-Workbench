using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Windows.Media;
using Cinch;
using ESME.Platform;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;
using ESME.Overlay;
using ESME.NEMO;
using System.IO;

namespace ESMEWorkBench.ViewModels.Layers
{
    public abstract class LayerViewModel : ViewModelBase
    {
        protected LayerViewModel(string name, string fileName, WpfMap wpfMap)
        {
            LayerName = name;
            FileName = fileName;
            WpfMap = wpfMap;
            Children = new LayersCollection();
            Children.CollectionChanged += Children_CollectionChanged;
            IsChecked = true;
        }

        void Children_CollectionChanged(object sender, System.Collections.Specialized.NotifyCollectionChangedEventArgs e)
        {
            //throw new System.NotImplementedException();
        }

        void Initialize()
        {
            foreach (var child in Children)
            {
                child._parent = this;
                child.Initialize();
            }
        }
        LayerViewModel _parent;

        public void MoveUp() { WpfMap.Overlays.MoveUp(Overlay); }
        public void MoveDown() { WpfMap.Overlays.MoveDown(Overlay); }
        public void MoveToTop() { WpfMap.Overlays.MoveToTop(Overlay); }
        public void MoveToBottom() { WpfMap.Overlays.MoveToBottom(Overlay); }
        public void MoveTo(int toIndex) { WpfMap.Overlays.MoveTo(Overlay, toIndex); }
        public int Index { get { return WpfMap.Overlays.IndexOf(Overlay); } }

        #region public string LayerName { get; set; }

        public string LayerName
        {
            get { return _layerName; }
            set
            {
                if (_layerName == value) return;
                _layerName = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }
        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.LayerName);
        string _layerName;

        #endregion

        #region public string FileName { get; set; }

        public string FileName
        {
            get { return _fileName; }
            set
            {
                if (_fileName == value) return;
                _fileName = value;
                NotifyPropertyChanged(FileNameChangedEventArgs);
            }
        }
        static readonly PropertyChangedEventArgs FileNameChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.FileName);
        string _fileName;

        #endregion

        #region public bool IsSelected { get; set; }

        public bool IsSelected
        {
            get { return _isSelected; }
            set
            {
                if (_isSelected == value) return;
                _isSelected = value;
                //Console.WriteLine("{0} IsSelected={1}", LayerName, _isSelected);
                NotifyPropertyChanged(IsSelectedChangedEventArgs);
            }
        }
        static readonly PropertyChangedEventArgs IsSelectedChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.IsSelected);
        bool _isSelected;

        #endregion

        #region public bool? IsChecked { get; set; }

        public bool? IsChecked
        {
            get { return _isChecked; }
            set { SetIsChecked(value, true, true); }
        }

        static readonly PropertyChangedEventArgs IsCheckedChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.IsChecked);
        bool? _isChecked = false;

        void SetIsChecked(bool? value, bool updateChildren, bool updateParent)
        {
            if (value == _isChecked)
                return;

            _isChecked = value;

            if (updateChildren && _isChecked.HasValue)
                foreach (var child in Children)
                    child.SetIsChecked(_isChecked, true, false);

            if (updateParent && _parent != null)
                _parent.VerifyCheckState();

            if (Overlay != null)
            {
                if ((_isChecked == null) || (!_isChecked.Value))
                    Overlay.IsVisible = false;
                else if (_isChecked.Value)
                    Overlay.IsVisible = true;
            }

            NotifyPropertyChanged(IsCheckedChangedEventArgs);
        }

        void VerifyCheckState()
        {
            bool? state = null;
            for (var i = 0; i < Children.Count; ++i)
            {
                var current = Children[i].IsChecked;
                if (i == 0)
                    state = current;
                else if (state != current)
                {
                    state = null;
                    break;
                }
            }
            SetIsChecked(state, false, true);
        }

        #endregion

        #region public LayersCollection Children { get; set; }

        public LayersCollection Children
        {
            get { return _children; }
            set
            {
                if (_children == value) return;
                _children = value;
                _children.CollectionChanged += ChildrenCollectionChanged;
                NotifyPropertyChanged(ChildrenChangedEventArgs);
            }
        }
        static readonly PropertyChangedEventArgs ChildrenChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.Children);
        LayersCollection _children;

        void ChildrenCollectionChanged(object sender, System.Collections.Specialized.NotifyCollectionChangedEventArgs e)
        {
            NotifyPropertyChanged(ChildrenChangedEventArgs);
        }

        #endregion

        public WpfMap WpfMap { get; set; }

        public Overlay Overlay { get; set; }
    }

    public abstract class LayerViewModel<T> : LayerViewModel where T : Layer
    {
        protected LayerViewModel(string name, string fileName, WpfMap wpfMap) : base(name, fileName, wpfMap) { }

        #region public T LayerData { get; set; }

        public T LayerData
        {
            get { return _layerData; }
            set
            {
                if (_layerData == value) return;
                _layerData = value;
                NotifyPropertyChanged(LayerChangedEventArgs);
            }
        }
        static readonly PropertyChangedEventArgs LayerChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel<T>>(x => x.LayerData);
        T _layerData;

        #endregion

    }

    public class LayersCollection : ObservableCollection<LayerViewModel>
    {
        public LayerViewModel this[string name] { get { return this.FirstOrDefault(cur => cur.LayerName == name); } }
    }

    public class ShapeLayerViewModel : LayerViewModel<InMemoryFeatureLayer>
    {
        public ShapeLayerViewModel(string name, OverlayShape shape, WpfMap wpfMap)
            : base(name, null, wpfMap)
        {

        }

        #region public Color Color { get; set; }

        public Color Color
        {
            get { return _color; }
            set
            {
                if (_color == value) return;
                _color = value;
                //Layer.ZoomLevelSet.ZoomLevel01.DefaultLineStyle.OuterPen = new GeoPen(GeoColor.FromArgb(Color.A, Color.R, Color.G, Color.B), LineWidth);
                NotifyPropertyChanged(ColorChangedEventArgs);
            }
        }
        static readonly PropertyChangedEventArgs ColorChangedEventArgs = ObservableHelper.CreateArgs<ShapeLayerViewModel>(x => x.Color);
        Color _color;

        #endregion

        #region public float LineWidth { get; set; }

        public float LineWidth
        {
            get { return _lineWidth; }
            set
            {
                if (_lineWidth == value) return;
                _lineWidth = value;
                //Layer.ZoomLevelSet.ZoomLevel01.DefaultLineStyle.OuterPen = new GeoPen(GeoColor.FromArgb(Color.A, Color.R, Color.G, Color.B), LineWidth);
                NotifyPropertyChanged(LineWidthChangedEventArgs);
            }
        }
        static readonly PropertyChangedEventArgs LineWidthChangedEventArgs = ObservableHelper.CreateArgs<ShapeLayerViewModel>(x => x.LineWidth);
        float _lineWidth;

        #endregion
    }

    public class ScenarioFileLayerViewModel : LayerViewModel
    {
        public ScenarioFileLayerViewModel(WpfMap wpfMap, string nemoFileName, string nemoScenarioDirectory)
            : base(Path.GetFileNameWithoutExtension(nemoFileName), nemoFileName, wpfMap)
        {
            Overlay = new LayerOverlay { TileType = TileType.SingleTile };
            WpfMap.Overlays.Add(Overlay);

            var nemoFile = new NemoFile(nemoFileName, nemoScenarioDirectory);

            Children = new LayersCollection();
            var overlayLayer = new OverlayShapesLayerViewModel(WpfMap, Overlay, Path.GetFileNameWithoutExtension(nemoFile.Scenario.OverlayFile.FileName));
            foreach (var shape in nemoFile.Scenario.OverlayFile.Shapes)
                overlayLayer.OverlayShapes.Add(shape);
            Children.Add(overlayLayer);

            var platformCount = 0;
            foreach (var platform in nemoFile.Scenario.Platforms)
            {
                var behavior = new BehaviorModel(platform);
                var platformLayer = new OverlayShapesLayerViewModel(WpfMap, Overlay, "Platform " + platformCount + ": " + platform.Name + " course");
                platformLayer.OverlayShapes.Add(behavior.CourseOverlay);
                platformLayer.OverlayShapes.Add(behavior.CourseStart);
                platformLayer.OverlayShapes.Add(behavior.CourseEnd);
                Children.Add(platformLayer);
                foreach (var trackdef in platform.Trackdefs)
                {
                    var opAreaLayer = new OverlayShapesLayerViewModel(WpfMap, Overlay, "Platform " + platformCount + ": " + platform.Name + " operational area");
                    foreach (var shape in trackdef.OverlayFile.Shapes)
                        opAreaLayer.OverlayShapes.Add(shape);
                    Children.Add(opAreaLayer);
                }
                platformCount++;
            }
            WpfMap.Refresh();
        }

    }

    public class ShapefileLayerViewModel : LayerViewModel
    {
        public ShapefileLayerViewModel(WpfMap wpfMap, string shapefileFileName)
            : base(Path.GetFileNameWithoutExtension(shapefileFileName), shapefileFileName, wpfMap)
        {
            Overlay = new LayerOverlay { TileType = TileType.SingleTile };
            WpfMap.Overlays.Add(Overlay);

            string projection = null;
            var projectionFile = Path.Combine(Path.GetDirectoryName(shapefileFileName), "projection.txt");
            if (File.Exists(projectionFile))
            {
                using (var sr = new StreamReader(projectionFile))
                    projection = sr.ReadToEnd();
            }
            var newLayer = new ShapeFileFeatureLayer(shapefileFileName);
            newLayer.ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = AreaStyles.County1;
            newLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            newLayer.RequireIndex = false;
            if (projection != null)
                newLayer.FeatureSource.Projection = new ManagedProj4Projection { InternalProjectionParameters = projection, ExternalProjectionParameters = ManagedProj4Projection.GetEpsgParameters(4326), };
            ((LayerOverlay)Overlay).Layers.Add(newLayer);
            WpfMap.Refresh();
        }
    }

    public class AdornmentLayerViewModel : LayerViewModel
    {
        public AdornmentLayerViewModel(WpfMap wpfMap, string name, AdornmentLayer adornmentLayer)
            : base(name, null, wpfMap)
        {
            Overlay = wpfMap.AdornmentOverlay;
            wpfMap.AdornmentOverlay.Layers.Add(adornmentLayer);
        }
    }

    public class OverlayShapesLayerViewModel : LayerViewModel
    {
        public OverlayShapesLayerViewModel(WpfMap wpfMap, string name, IEnumerable<OverlayShape> shapes)
            : base(name, null, wpfMap)
        {
            OverlayShapes = new ObservableCollection<OverlayShape>();
            ShapeLayers = new ObservableCollection<InMemoryFeatureLayer>();

            foreach (var shape in shapes)
                OverlayShapes.Add(shape);
        }

        public OverlayShapesLayerViewModel(WpfMap wpfMap, Overlay layerOverlay, string name)
            : base(name, null, wpfMap)
        {
            OverlayShapes = new ObservableCollection<OverlayShape>();
            ShapeLayers = new ObservableCollection<InMemoryFeatureLayer>();
            if (layerOverlay != null)
                Overlay = layerOverlay;
        }

        #region public ObservableCollection<InMemoryFeatureLayer> ShapeLayers { get; set; }

        public ObservableCollection<InMemoryFeatureLayer> ShapeLayers
        {
            get { return _shapeLayers; }
            set
            {
                if (_shapeLayers == value) return;
                _shapeLayers = value;
                _shapeLayers.CollectionChanged += ShapeLayersCollectionChanged;
                NotifyPropertyChanged(ShapeLayersChangedEventArgs);
            }
        }
        static readonly PropertyChangedEventArgs ShapeLayersChangedEventArgs = ObservableHelper.CreateArgs<OverlayShapesLayerViewModel>(x => x.ShapeLayers);
        ObservableCollection<InMemoryFeatureLayer> _shapeLayers;
        void ShapeLayersCollectionChanged(object sender, System.Collections.Specialized.NotifyCollectionChangedEventArgs e)
        {
            if (e.NewItems != null)
            {
                foreach (var item in e.NewItems)
                {
                    var newLayer = (InMemoryFeatureLayer)item;
                    ((LayerOverlay)Overlay).Layers.Add(newLayer);
                }
                //LayerOverlay.Refresh();
            }
            if (e.OldItems != null)
            {
                foreach (var item in e.OldItems)
                {
                    var oldLayer = (InMemoryFeatureLayer)item;
                    ((LayerOverlay)Overlay).Layers.Remove(oldLayer);
                }
                //LayerOverlay.Refresh();
            }
            NotifyPropertyChanged(ShapeLayersChangedEventArgs);
            WpfMap.Refresh();
        }

        #endregion

        #region public ObservableCollection<InMemoryFeatureLayer> OverlayShapes { get; set; }

        public ObservableCollection<OverlayShape> OverlayShapes
        {
            get { return _overlayShapes; }
            set
            {
                if (_overlayShapes == value) return;
                _overlayShapes = value;
                _overlayShapes.CollectionChanged += OverlayShapesCollectionChanged;
                NotifyPropertyChanged(OverlayShapesChangedEventArgs);
            }
        }
        static readonly PropertyChangedEventArgs OverlayShapesChangedEventArgs = ObservableHelper.CreateArgs<OverlayShapesLayerViewModel>(x => x.OverlayShapes);
        ObservableCollection<OverlayShape> _overlayShapes;
        void OverlayShapesCollectionChanged(object sender, System.Collections.Specialized.NotifyCollectionChangedEventArgs e)
        {
            foreach (var item in e.NewItems)
            {
                var shape = (OverlayShape)item;
                var newLayer = new InMemoryFeatureLayer();
                newLayer.InternalFeatures.Add(new Feature(BaseShape.CreateShapeFromWellKnownData(shape.WellKnownText)));
                newLayer.ZoomLevelSet.ZoomLevel01.DefaultLineStyle.OuterPen = new GeoPen(GeoColor.FromArgb(shape.Color.A, shape.Color.R, shape.Color.G, shape.Color.B), shape.Width);
                newLayer.ZoomLevelSet.ZoomLevel01.DefaultPointStyle.SymbolPen = new GeoPen(GeoColor.FromArgb(shape.Color.A, shape.Color.R, shape.Color.G, shape.Color.B), shape.Width);
                newLayer.ZoomLevelSet.ZoomLevel01.DefaultPointStyle.SymbolSize = shape.Width;
                newLayer.ZoomLevelSet.ZoomLevel01.DefaultPointStyle.SymbolType = PointSymbolType.Circle;
                newLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
                ShapeLayers.Add(newLayer);
            }
            NotifyPropertyChanged(OverlayShapesChangedEventArgs);
        }

        #endregion
    }

    public class OverlayFileLayerViewModel : OverlayShapesLayerViewModel
    {
        public OverlayFileLayerViewModel(WpfMap wpfMap, string overlayFileName)
            : base(wpfMap, null, Path.GetFileNameWithoutExtension(overlayFileName))
        {
            Overlay = new LayerOverlay { TileType = TileType.SingleTile };
            WpfMap.Overlays.Add(Overlay);

            var overlayFile = new OverlayFile(overlayFileName);
            foreach (var s in overlayFile.Shapes)
                OverlayShapes.Add(s);
        }
    }
}
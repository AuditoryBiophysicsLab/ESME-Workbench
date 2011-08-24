using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text.RegularExpressions;
using System.Windows;
using Cinch;
using ESME.Metadata;
using ESME.NEMO;
using HRC.Utility;
using HRC.ViewModels;

namespace ESME.Mapping
{
    public class TreeNode : PropertyChangedBase, IHaveProperties
    {
        public TreeNode()
        {
            MapLayers = new ObservableList<MapLayerViewModel>();
            Nodes = new ObservableList<TreeNode>();
        }

        public TreeNode(string format, params object[] args) : this() { Name = string.Format(format, args); }

        public void RemoveMapLayer(MapLayerViewModel mapLayer)
        {
            if (MapLayerMatchesMe(mapLayer))
                MapLayers.Remove(mapLayer);
            foreach (var node in Nodes)
                node.RemoveMapLayer(mapLayer);
        }

        public void AddMapLayer(MapLayerViewModel mapLayer)
        {
            if (MapLayerMatchesMe(mapLayer))
            {
                var existingLayer = MapLayers.Find(layer => layer.Name == mapLayer.Name);
                if (existingLayer != null) MapLayers[MapLayers.IndexOf(existingLayer)] = mapLayer;
                else MapLayers.Add(mapLayer);
            }
            foreach (var node in Nodes)
                node.AddMapLayer(mapLayer);
        }

        protected virtual bool MapLayerMatchesMe(MapLayerViewModel mapLayer) { return false; }

        #region public virtual string ToolTipTitle { get; set; }

        public virtual string ToolTipTitle
        {
            get { return _toolTipTitle ?? Name; }
            set
            {
                if (_toolTipTitle == value) return;
                _toolTipTitle = value;
                NotifyPropertyChanged(ToolTipTitleChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ToolTipTitleChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.ToolTipTitle);
        string _toolTipTitle;

        #endregion

        #region public virtual IEnumerable<KeyValuePair<string, string>> ToolTipProperties { get; set; }

        public virtual IEnumerable<KeyValuePair<string, string>> ToolTipProperties
        {
            get { return _toolTipProperties; }
            set
            {
                if (_toolTipProperties == value) return;
                _toolTipProperties = value;
                NotifyPropertyChanged(ToolTipPropertiesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ToolTipPropertiesChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.ToolTipProperties);
        IEnumerable<KeyValuePair<string, string>> _toolTipProperties;

        #endregion

        #region public virtual string Name { get; set; }

        public virtual string Name
        {
            get { return _name; }
            private set
            {
                if (_name == value) return;
                _name = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }

        protected static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.Name);
        string _name;

        #endregion

        #region public IEnumerable<object> Children { get; set; }

        public IEnumerable<object> Children
        {
            get
            {
                foreach (var layer in MapLayers) yield return layer;
                foreach (var node in Nodes) yield return node;
            }
        }
        static readonly PropertyChangedEventArgs ChildrenChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.Children);

        #endregion

        #region public ObservableList<MapLayerViewModel> MapLayers { get; set; }

        public ObservableList<MapLayerViewModel> MapLayers
        {
            get { return _mapLayers; }
            set
            {
                if (_mapLayers == value) return;
                _mapLayers = value;
                NotifyPropertyChanged(MapLayersChangedEventArgs);
                if (_mapLayers != null) _mapLayers.CollectionChanged += (s, e) =>
                {
                    NotifyPropertyChanged(NameChangedEventArgs);
                    NotifyPropertyChanged(ChildrenChangedEventArgs);
                };
            }
        }

        static readonly PropertyChangedEventArgs MapLayersChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.MapLayers);
        ObservableList<MapLayerViewModel> _mapLayers;

        #endregion

        #region public ObservableList<TreeNode> Nodes { get; set; }

        public ObservableList<TreeNode> Nodes
        {
            get { return _nodes; }
            set
            {
                if (_nodes == value) return;
                _nodes = value;
                NotifyPropertyChanged(NodesChangedEventArgs);
                NotifyPropertyChanged(ChildrenChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NodesChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.Nodes);
        ObservableList<TreeNode> _nodes;

        #endregion

        #region public ObservableList<MenuItemViewModelBase> ContextMenu { get; set; }

        public ObservableList<MenuItemViewModelBase> ContextMenu
        {
            get { return _contextMenu; }
            set
            {
                if (_contextMenu == value) return;
                _contextMenu = value;
                NotifyPropertyChanged(ContextMenuChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ContextMenuChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.ContextMenu);
        ObservableList<MenuItemViewModelBase> _contextMenu;

        #endregion

        #region public bool HasContextMenu { get; set; }

        public bool HasContextMenu
        {
            get { return _hasContextMenu; }
            set
            {
                if (_hasContextMenu == value) return;
                _hasContextMenu = value;
                NotifyPropertyChanged(HasContextMenuChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs HasContextMenuChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.HasContextMenu);
        bool _hasContextMenu;

        #endregion

        #region public string PropertyViewName { get; set; }

        public string PropertyViewName
        {
            get { return _propertyViewName; }
            set
            {
                if (_propertyViewName == value) return;
                _propertyViewName = value;
                NotifyPropertyChanged(PropertyViewNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PropertyViewNameChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.PropertyViewName);
        string _propertyViewName;

        #endregion

        #region public string WindowTitle { get; set; }

        public string WindowTitle
        {
            get { return _windowTitle; }
            set
            {
                if (_windowTitle == value) return;
                _windowTitle = value;
                NotifyPropertyChanged(WindowTitleChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WindowTitleChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.WindowTitle);
        string _windowTitle;

        #endregion

        #region public List<KeyValuePair<string, string>> KeyValuePairs { get; protected set; }

        public List<KeyValuePair<string, string>> KeyValuePairs
        {
            get { return _keyValuePairs; }
            protected set
            {
                if (_keyValuePairs == value) return;
                _keyValuePairs = value;
                NotifyPropertyChanged(KeyValuePairsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs KeyValuePairsChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.KeyValuePairs);
        List<KeyValuePair<string, string>> _keyValuePairs;

        #endregion

    }

    public class TreeNodeWrapper<T> : TreeNode
    {
        protected MenuItemViewModelBase PropertiesMenu { get; set; }
        public TreeNodeWrapper()
        {
            PropertiesMenu = new MenuItemViewModelBase
            {
                Header = "Properties...",
                Command = new SimpleCommand<object, object>(delegate { MediatorMessage.Send(MediatorMessage.ShowProperties, this); }),
            };
            ContextMenu = new ObservableList<MenuItemViewModelBase> {PropertiesMenu};

            PropertyViewName = "BasicPropertiesView";
        }
        public TreeNodeWrapper(T wrappedObject) : this()
        {
            WrappedObject = wrappedObject;
            if (wrappedObject is IHaveProperties) PropertiesMenu.Visibility = Visibility.Visible;
        }
        public T WrappedObject { get; protected set; }
    }

    public class EnvironmentNode : TreeNode
    {
        public EnvironmentNode(string format, params object[] args) : base(format, args)
        {
            WindowTitle = "Environment properties";
        }

        protected override bool MapLayerMatchesMe(MapLayerViewModel mapLayer)
        {
            return mapLayer.LayerType == LayerType.BaseMap || mapLayer.LayerType == LayerType.Bathymetry ||
                   mapLayer.LayerType == LayerType.BathymetryRaster || mapLayer.LayerType == LayerType.BottomType ||
                   mapLayer.LayerType == LayerType.SoundSpeed || mapLayer.LayerType == LayerType.WindSpeed;
        }
    }

    public class AnalysisPointNode : TreeNode
    {
        public AnalysisPointNode(string format, params object[] args) : base(format, args) { }

        protected override bool MapLayerMatchesMe(MapLayerViewModel mapLayer) { return mapLayer.LayerType == LayerType.AnalysisPoint; }
    }

    public class ScenarioNode : TreeNodeWrapper<NemoScenario>
    {
        public ScenarioNode(NemoScenario nemoScenario) : base(nemoScenario)
        {
            WindowTitle = "Scenario Properties: " + nemoScenario.EventName;
            KeyValuePairs = nemoScenario.Properties.ToList();
            HasContextMenu = true;
            foreach (var platform in nemoScenario.Platforms)
                Nodes.Add(new PlatformNode(platform));
        }

        public override string Name { get { return string.Format("Scenario: {0}", WrappedObject.EventName); } }
        protected override bool MapLayerMatchesMe(MapLayerViewModel mapLayer) { return mapLayer.LayerType == LayerType.Animal; }
    }

    public class PlatformNode : TreeNodeWrapper<NemoPlatform>
    {
        public PlatformNode(NemoPlatform nemoPlatform) : base(nemoPlatform)
        {
            WindowTitle = "Platform Properties: " + nemoPlatform.Name;
            KeyValuePairs = nemoPlatform.Properties.ToList();
            HasContextMenu = true;
            foreach (var source in nemoPlatform.Sources)
                Nodes.Add(new SourceNode(source));
        }

        protected override bool MapLayerMatchesMe(MapLayerViewModel mapLayer) { return new Regex(string.Format(@"{0} [\s\S]+$", Name)).IsMatch(mapLayer.Name); }

        public override string Name { get { return string.Format("Platform: {0}", WrappedObject.Name); } }
    }

    public class SourceNode : TreeNodeWrapper<NemoSource>
    {
        public SourceNode(NemoSource nemoSource) : base(nemoSource)
        {
            WindowTitle = "Source Properties: " + nemoSource.Name;
            KeyValuePairs = nemoSource.Properties.ToList();
            HasContextMenu = true;
            foreach (var mode in nemoSource.Modes)
                Nodes.Add(new ModeNode(mode));
        }

        public override string Name { get { return string.Format("Source: {0}", WrappedObject.Name); } }
    }

    public class ModeNode : TreeNodeWrapper<NemoMode>
    {
        public ModeNode(NemoMode nemoMode) : base(nemoMode)
        {
            WindowTitle = "Mode Properties: " + nemoMode.Name;
            KeyValuePairs = nemoMode.Properties.ToList();
            HasContextMenu = true;
        }

        protected override bool MapLayerMatchesMe(MapLayerViewModel mapLayer) { return mapLayer.Name.Contains(WrappedObject.PSMName); }

        public override string Name { get { return string.Format("Mode: {0} [{1}]", WrappedObject.Name, MapLayers.Count); } }
    }
}
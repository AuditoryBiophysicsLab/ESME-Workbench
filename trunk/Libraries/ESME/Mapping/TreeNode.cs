using System.Collections.Generic;
using System.ComponentModel;
using System.Text.RegularExpressions;
using Cinch;
using ESME.NEMO;
using HRC.Utility;
using HRC.ViewModels;

namespace ESME.Mapping
{
    public class TreeNode : PropertyChangedBase
    {
        public TreeNode()
        {
            MapLayers = new ObservableList<MapLayerViewModel>();
            Nodes = new ObservableList<TreeNode>();
        }

        public TreeNode(string format, params object[] args) : this() { Name = string.Format(format, args); }

        public TreeNode FindNodeForMapLayer(MapLayerViewModel mapLayer)
        {
            if (MapLayerMatchesMe(mapLayer))
            {
                var existingLayer = MapLayers.Find(layer => layer.Name == mapLayer.Name);
                if (existingLayer != null) MapLayers[MapLayers.IndexOf(existingLayer)] = mapLayer;
                else MapLayers.Add(mapLayer);
                return this;
            }
            foreach (var node in Nodes)
            {
                var result = node.FindNodeForMapLayer(mapLayer);
                if (result != null) return result;
            }
            return null;
        }

        protected virtual bool MapLayerMatchesMe(MapLayerViewModel mapLayer) { return false; }

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
                if (_mapLayers != null) _mapLayers.CollectionChanged += (s, e) => NotifyPropertyChanged(NameChangedEventArgs);
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
                NotifyPropertyChanged(ChildrenChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ChildrenChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.Nodes);
        ObservableList<TreeNode> _nodes;

        #endregion

        #region public List<MenuItemViewModelBase> ContextMenu { get; set; }

        public List<MenuItemViewModelBase> ContextMenu
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
        List<MenuItemViewModelBase> _contextMenu;

        #endregion
    }

    public class TreeNodeWrapper<T> : TreeNode
    {
        public TreeNodeWrapper() {  }
        public TreeNodeWrapper(T wrappedObject) { WrappedObject = wrappedObject; }
        public T WrappedObject { get; protected set; }
    }

    public class ScenarioNode : TreeNodeWrapper<NemoScenario>
    {
        public ScenarioNode(NemoScenario nemoScenario) : base(nemoScenario)
        {
            TreeNode nodesRoot;
            if (nemoScenario.Platforms.Count < 3) nodesRoot = this;
            else
            {
                nodesRoot = new TreeNode("Platforms");
                Nodes.Add(nodesRoot);
            }
            foreach (var platform in nemoScenario.Platforms)
                nodesRoot.Nodes.Add(new PlatformNode(platform));
        }

        public override string Name { get { return string.Format("Scenario: {0}", WrappedObject.EventName); } }
    }

    public class PlatformNode : TreeNodeWrapper<NemoPlatform>
    {
        public PlatformNode(NemoPlatform nemoPlatform) : base(nemoPlatform)
        {
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
            foreach (var mode in nemoSource.Modes)
                Nodes.Add(new ModeNode(mode));
        }

        public override string Name { get { return string.Format("Source: {0}", WrappedObject.Name); } }
    }

    public class ModeNode : TreeNodeWrapper<NemoMode>
    {
        public ModeNode(NemoMode nemoMode) : base(nemoMode)
        {
        }

        protected override bool MapLayerMatchesMe(MapLayerViewModel mapLayer) { return mapLayer.Name.Contains(WrappedObject.PSMName); }

        public override string Name { get { return string.Format("Mode: {0} [{1}]", WrappedObject.Name, MapLayers.Count); } }
    }
}
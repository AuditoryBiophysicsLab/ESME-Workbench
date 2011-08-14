using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text.RegularExpressions;
using Cinch;
using ESME.NEMO;
using HRC.Utility;
using HRC.ViewModels;

namespace ESME.Mapping
{
    public class TreeNode : INotifyPropertyChanged
    {
        public TreeNode()
        {
            MapLayers = new ObservableList<MapLayerViewModel>();
            Nodes = new TreeNodeList();
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

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.Name);
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

        #region public List<MapLayerViewModel> MapLayers { get; set; }

        public List<MapLayerViewModel> MapLayers
        {
            get { return _mapLayers; }
            set
            {
                if (_mapLayers == value) return;
                _mapLayers = value;
                NotifyPropertyChanged(MapLayersChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MapLayersChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.MapLayers);
        List<MapLayerViewModel> _mapLayers;

        #endregion

        #region public TreeNodeList Nodes { get; set; }

        public TreeNodeList Nodes
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
        TreeNodeList _nodes;

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

        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }

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
            TreeNode nodesRoot;
            if (nemoPlatform.Sources.Count < 3) nodesRoot = this;
            else
            {
                nodesRoot = new TreeNode("Sources");
                Nodes.Add(nodesRoot);
            }
            foreach (var source in nemoPlatform.Sources)
                nodesRoot.Nodes.Add(new SourceNode(source));
        }

        protected override bool MapLayerMatchesMe(MapLayerViewModel mapLayer) { return new Regex(string.Format(@"{0} [\s\S]+$", Name)).IsMatch(mapLayer.Name); }

        public override string Name { get { return string.Format("Platform: {0}", WrappedObject.Name); } }
    }

    public class SourceNode : TreeNodeWrapper<NemoSource>
    {
        public SourceNode(NemoSource nemoSource) : base(nemoSource)
        {
            TreeNode nodesRoot;
            if (nemoSource.Modes.Count < 3) nodesRoot = this;
            else
            {
                nodesRoot = new TreeNode("Modes");
                Nodes.Add(nodesRoot);
            }
            foreach (var mode in nemoSource.Modes)
                nodesRoot.Nodes.Add(new ModeNode(mode));
        }

        public override string Name { get { return string.Format("Source: {0}", WrappedObject.Name); } }
    }

    public class ModeNode : TreeNodeWrapper<NemoMode>
    {
        public ModeNode(NemoMode nemoMode) : base(nemoMode)
        {
        }

        protected override bool MapLayerMatchesMe(MapLayerViewModel mapLayer)
        {
            if ((mapLayer.LayerType == LayerType.Propagation) && (!Nodes.Any(node => node.Name == "Propagation"))) Nodes.Add(new TransmissionLossContainerNode("Propagation", WrappedObject.PSMName));
            if ((mapLayer.LayerType == LayerType.Pressure) && (!Nodes.Any(node => node.Name == "Pressure"))) Nodes.Add(new TransmissionLossContainerNode("Pressure", WrappedObject.PSMName));
            return false;
        }

        public override string Name { get { return string.Format("Mode: {0}", WrappedObject.Name); } }
    }

    public class TransmissionLossContainerNode : TreeNode
    {
        public TransmissionLossContainerNode(string layerName, string psmName)
        {
            _layerName = layerName;
            _psmName = psmName; 
        }

        readonly string _psmName;
        readonly string _layerName;
        public override string Name { get { return _layerName; } }
        protected override bool MapLayerMatchesMe(MapLayerViewModel mapLayer) { return mapLayer.Name.Contains(_psmName); }
    }
}
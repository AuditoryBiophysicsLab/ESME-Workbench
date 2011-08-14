using System.Collections.Specialized;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Text.RegularExpressions;
using Cinch;
using HRC.Utility;
using HRC.ViewModels;

namespace ESME.Mapping
{
    public class TreeNode : ViewModelBase, IHaveAName
    {
        public TreeNode()
        {
            MapLayers = new ObservableList<MapLayerViewModel>();
            Children = new TreeNodeList();
        }
        #region public string Name { get; set; }

        public string Name
        {
            get { return _name; }
            set
            {
                if (_name == value) return;
                _name = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.Name);
        string _name;

        #endregion

        #region public  ObservableList<MapLayerViewModel> MapLayers { get; set; }

        public ObservableList<MapLayerViewModel> MapLayers
        {
            get { return _mapLayers ?? (_mapLayers = new ObservableList<MapLayerViewModel>()); }
            set
            {
                if (_mapLayers == value) return;
                if (_mapLayers != null) _mapLayers.CollectionChanged -= MapLayersCollectionChanged;
                _mapLayers = value;
                if (_mapLayers != null) _mapLayers.CollectionChanged += MapLayersCollectionChanged;
                NotifyPropertyChanged(MapLayersChangedEventArgs);
            }
        }

        void MapLayersCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    if (e.NewItems != null)
                        foreach (MapLayerViewModel item in e.NewItems)
                        {
                            Children.Add(item);
                        }
                    break;
                case NotifyCollectionChangedAction.Move:
                    Debug.WriteLine("TreeNode: MapLayers.Move");
                    break;
                case NotifyCollectionChangedAction.Remove:
                        foreach (MapLayerViewModel item in e.OldItems)
                        {
                            Children.Remove(item);
                        }
                    break;
                case NotifyCollectionChangedAction.Replace:
                        foreach (MapLayerViewModel item in e.OldItems)
                        {
                            Children.Remove(item);
                        }
                        foreach (MapLayerViewModel item in e.NewItems)
                        {
                            Children.Add(item);
                        }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    Debug.WriteLine("TreeNode: MapLayers.Reset");
                    break;
            }
        }
        static readonly PropertyChangedEventArgs MapLayersChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.MapLayers);
        ObservableList<MapLayerViewModel> _mapLayers;

        public void UpdateMapLayers(MapLayerCollection mapLayerSource, LayerType layerType, string layerName)
        {
            if (MapLayers == null) MapLayers = new ObservableList<MapLayerViewModel>();
            var target = mapLayerSource.Find(layerType, layerName).FirstOrDefault();
            if (target == null)
            {
                MapLayers.RemoveLayerType(layerType);
            }
            else
            {
                target.TreeViewParent = this;
                MapLayers.AddOrReplace(layerType, layerName, target);
            }
        }

        public void UpdateMapLayers(MapLayerCollection mapLayerSource, LayerType layerType, Regex nameRegex)
        {
            if (MapLayers == null) MapLayers = new ObservableList<MapLayerViewModel>();
            var targets = mapLayerSource.Find(layerType, nameRegex);
            if (targets == null)
            {
                MapLayers.RemoveAll(layer => (layer.LayerType == layerType) && (nameRegex.IsMatch(layer.Name)));
            }
            else
            {
                foreach (var target in targets)
                {
                    target.TreeViewParent = this;
                    MapLayers.AddOrReplace(layerType, target.Name, target);
                }
            }
        }

        #endregion

        #region public TreeNodeList Children { get; set; }

        public TreeNodeList Children
        {
            get { return _children; }
            set
            {
                if (_children == value) return;
                _children = value;
                NotifyPropertyChanged(ChildrenChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ChildrenChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.Children);
        TreeNodeList _children;

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

        #region public string ClosedImageName { get; set; }

        public string ClosedImageName
        {
            get { return _closedImageName; }
            set
            {
                if (_closedImageName == value) return;
                _closedImageName = value;
                NotifyPropertyChanged(ClosedImageNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ClosedImageNameChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.ClosedImageName);
        string _closedImageName;

        #endregion

        #region public string OpenImageName { get; set; }

        public string OpenImageName
        {
            get { return _openImageName; }
            set
            {
                if (_openImageName == value) return;
                _openImageName = value;
                NotifyPropertyChanged(OpenImageNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OpenImageNameChangedEventArgs = ObservableHelper.CreateArgs<TreeNode>(x => x.OpenImageName);
        string _openImageName;

        #endregion
    }
}
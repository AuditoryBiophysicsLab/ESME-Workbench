using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Windows.Controls;
using Cinch;
using HRC.Utility;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Mapping
{
    [ExportViewModel("LayerListViewModel")]
    [PartCreationPolicy(CreationPolicy.Shared)]
    public class LayerListViewModel : ViewModelBase
    {
        #region Private fields

        readonly IViewAwareStatus _viewAwareStatus;
        readonly IMessageBoxService _messageBoxService;

        #endregion

        [ImportingConstructor]
        public LayerListViewModel(IViewAwareStatus viewAwareStatus, IMessageBoxService messageBoxService)
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nLayerListViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
            _viewAwareStatus = viewAwareStatus;
            _messageBoxService = messageBoxService;

            _viewAwareStatus.ViewLoaded += ViewLoaded;
        }

        #region public MapLayerCollection MapLayers { get; set; }

        public MapLayerCollection MapLayers
        {
            get { return _mapLayers; }
            set
            {
                if (_mapLayers == value) return;
                _mapLayers = value;
                NotifyPropertyChanged(MapLayersChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MapLayersChangedEventArgs = ObservableHelper.CreateArgs<LayerListViewModel>(x => x.MapLayers);
        MapLayerCollection _mapLayers;

        #endregion

        #region public ObservableList<TreeNode> TreeViewRootNodes { get; set; }

        public ObservableList<TreeNode> TreeViewRootNodes
        {
            get { return _treeViewRootNodes; }
            set
            {
                if (_treeViewRootNodes == value) return;
                _treeViewRootNodes = value;
                NotifyPropertyChanged(TreeViewRootNodesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TreeViewRootNodesChangedEventArgs = ObservableHelper.CreateArgs<LayerListViewModel>(x => x.TreeViewRootNodes);
        ObservableList<TreeNode> _treeViewRootNodes;

        #endregion

        [MediatorMessageSink(MediatorMessage.SetTreeRoots)]
        void SetTreeRoots(ObservableList<TreeNode> rootNodes) { TreeViewRootNodes = rootNodes; }

        [MediatorMessageSink(MediatorMessage.SetMapLayers)]
        void SetMapLayers(MapLayerCollection mapLayers) { MapLayers = mapLayers; }

        static void ViewLoaded()
        {
            MediatorMessage.Send(MediatorMessage.LayerListViewModelInitialized);
        }

#if false
        [MediatorMessageSink(MediatorMessage.RemoveLayer)]
        void RemoveLayer(MapLayerViewModel layer)
        {
            MapLayers.Remove(layer);
            if (layer.LayerType == LayerType.AnalysisPoint) MediatorMessage.Send(MediatorMessage.RemoveAnalysisPoint, layer.AnalysisPoint);
        }

        [MediatorMessageSink(MediatorMessage.LayersReordered)]
        void ReorderLayer(MapLayerViewModel layer)
        {
            var layerIndex = MapLayers.IndexOf(layer);
            if (layerIndex == -1) return;
            MapLayers.Move(layerIndex, layer.Index);
        }
#endif

        [MediatorMessageSink(MediatorMessage.EnableGUI)]
        void EnableGUI(bool enable)
        {
            ((UserControl)_viewAwareStatus.View).IsEnabled = enable;
        }
    }
}
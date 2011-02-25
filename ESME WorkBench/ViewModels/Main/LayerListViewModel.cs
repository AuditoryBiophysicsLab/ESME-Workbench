using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Windows;
using System.Windows.Controls;
using Cinch;
using ESMEWorkBench.Data;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.Map;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.Main
{
    [ExportViewModel("LayerListViewModel")]
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
                Debug.WriteLine("***********\nLayerTreeViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
            _viewAwareStatus = viewAwareStatus;
            _messageBoxService = messageBoxService;

            _viewAwareStatus.ViewLoaded += ViewLoaded;

            MapLayers = new ObservableCollection<MapLayerViewModel>();
        }

        #region public ObservableCollection<layerOverlayViewModel> MapLayers { get; set; }

        public ObservableCollection<MapLayerViewModel> MapLayers
        {
            get { return _layerViewModels; }
            set
            {
                if (_layerViewModels == value) return;
                if (_layerViewModels != null) _layerViewModels.CollectionChanged -= ShapeMapLayersCollectionChanged;
                _layerViewModels = value;
                if (_layerViewModels != null) _layerViewModels.CollectionChanged += ShapeMapLayersCollectionChanged;
                NotifyPropertyChanged(MapLayersChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MapLayersChangedEventArgs = ObservableHelper.CreateArgs<LayerListViewModel>(x => x.MapLayers);
        ObservableCollection<MapLayerViewModel> _layerViewModels;

        void ShapeMapLayersCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(MapLayersChangedEventArgs); }

        #endregion
        #region public Experiment Experiment { get; set; }

        public Experiment Experiment
        {
            get { return _experiment; }
            set
            {
                if (_experiment == value) return;
                _experiment = value;
                NotifyPropertyChanged(ExperimentChangedEventArgs);
                OnExperimentChanged(_experiment);
            }
        }

        static readonly PropertyChangedEventArgs ExperimentChangedEventArgs = ObservableHelper.CreateArgs<MapViewModel>(x => x.Experiment);
        Experiment _experiment;
        void OnExperimentChanged(Experiment experiment) 
        {
            MapLayers = experiment == null ? null : experiment.MapLayers;
        }

        #endregion

        [MediatorMessageSink(MediatorMessage.SetExperiment)]
        void SetExperiment(Experiment experiment)
        {
            Experiment = experiment;
        }

        static void ViewLoaded()
        {
            MediatorMessage.Send(MediatorMessage.LayerListViewModelInitialized);
        }

        [MediatorMessageSink(MediatorMessage.SetLayerCollection)]
        void SetLayerCollection(ObservableCollection<MapLayerViewModel> mapLayers) { MapLayers = mapLayers; }

        [MediatorMessageSink(MediatorMessage.RemoveLayer)]
        void RemoveLayer(MapLayerViewModel layer)
        {
            MapLayers.Remove(layer);
            if (layer.LayerType == LayerType.AnalysisPoint) MediatorMessage.Send(MediatorMessage.RemoveAnalysisPoint, layer.AnalysisPoint);
        }

        [MediatorMessageSink(MediatorMessage.LayersReordered)]
        void ReorderLayer(MapLayerViewModel layer) 
        {
            MapLayers.Move(MapLayers.IndexOf(layer), layer.Index); 
        }

        [MediatorMessageSink(MediatorMessage.EnableGUI)]
        void EnableGUI(bool enable)
        {
            ((UserControl)_viewAwareStatus.View).IsEnabled = enable;
        }
    }
}
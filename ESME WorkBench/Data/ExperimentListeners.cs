using Cinch;

namespace ESMEWorkBench.Data
{
    public partial class Experiment
    {
        static bool _mainViewModelInitialized;
        static bool _mapViewModelInitialized;
        static bool _layerTreeViewModelInitialized;

        [MediatorMessageSink("MainViewModelInitializedMessage")]
        void MainViewModelInitialized(bool dummy)
        {
            _mainViewModelInitialized = true;
            InitializeIfViewModelsReady();
        }

        [MediatorMessageSink("MapViewModelInitializedMessage")]
        void MapViewModelInitialized(bool dummy)
        {
            _mapViewModelInitialized = true;
            InitializeIfViewModelsReady();
        }

        [MediatorMessageSink("LayerTreeViewModelInitializedMessage")]
        void LayerTreeViewModelInitialized(bool dummy)
        {
            _layerTreeViewModelInitialized = true;
            InitializeIfViewModelsReady();
        }
    }
}
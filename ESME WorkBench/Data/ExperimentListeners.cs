using System;
using Cinch;
using ESME.NEMO;

namespace ESMEWorkBench.Data
{
    public partial class Experiment
    {
        static bool _mainViewModelInitialized;
        static bool _mapViewModelInitialized;
        static bool _layerListViewModelInitialized;

        [MediatorMessageSink(MediatorMessage.MainViewModelInitialized)]
        void MainViewModelInitialized(bool dummy)
        {
            _mainViewModelInitialized = true;
            InitializeIfViewModelsReady();
        }

        [MediatorMessageSink(MediatorMessage.MapViewModelInitialized)]
        void MapViewModelInitialized(bool dummy)
        {
            _mapViewModelInitialized = true;
            InitializeIfViewModelsReady();
        }

        [MediatorMessageSink(MediatorMessage.LayerListViewModelInitialized)]
        void LayerListViewModelInitialized(bool dummy)
        {
            _layerListViewModelInitialized = true;
            InitializeIfViewModelsReady();
        }

        [MediatorMessageSink(MediatorMessage.ExperimentLoaded)]
        void ExperimentLoaded(bool dummy)
        {
            InitializeIfViewModelsReady();
        }

        [MediatorMessageSink(MediatorMessage.AddShapefile)]
        void AddShapefileCommand(string fileName)
        {
            MediatorMessage.Send(MediatorMessage.AddShapefileCommand, fileName);
        }

        [MediatorMessageSink(MediatorMessage.AddOverlayFile)]
        void AddOverlayFileCommand(string fileName)
        {
            MediatorMessage.Send(MediatorMessage.AddOverlayFileCommand, fileName);
        }

        [MediatorMessageSink(MediatorMessage.AddScenarioFile)]
        void AddScenarioFileCommand(string fileName)
        {
            if (fileName != null)
            {
                try
                {
                    ScenarioFileName = fileName;
                    NemoFile = new NemoFile(fileName, Globals.AppSettings.ScenarioDataDirectory);
                    MediatorMessage.Send(MediatorMessage.AddScenarioFileCommand, NemoFile);
                }
                catch (Exception e)
                {
                    MessageBoxService.ShowError("Error opening scenario file:\n" + e.Message);
                }
            }
        }
    }
}
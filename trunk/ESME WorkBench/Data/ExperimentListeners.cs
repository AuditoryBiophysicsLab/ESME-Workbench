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
            IsChanged = false;
            InitializeIfViewModelsReady();
        }

        [MediatorMessageSink(MediatorMessage.AddShapefileCommand)]
        void AddShapefileCommand(string fileName)
        {
            MediatorMessage.Send(MediatorMessage.AddFileLayer, fileName);
        }

        [MediatorMessageSink(MediatorMessage.AddOverlayFileCommand)]
        void AddOverlayFileCommand(string fileName)
        {
            MediatorMessage.Send(MediatorMessage.AddFileLayer, fileName);
        }

        [MediatorMessageSink(MediatorMessage.AddEnvironmentFileCommand)]
        void AddEnvironmentFileCommand(string fileName)
        {
            MediatorMessage.Send(MediatorMessage.AddFileLayer, fileName);
        }

        [MediatorMessageSink(MediatorMessage.AddScenarioFileCommand)]
        void AddScenarioFileCommand(string fileName)
        {
            if (fileName != null)
            {
                try
                {
                    ScenarioFileName = fileName;
                    NemoFile = new NemoFile(fileName, Globals.AppSettings.ScenarioDataDirectory);
                    MediatorMessage.Send(MediatorMessage.AddScenarioLayer, NemoFile);
                }
                catch (Exception e)
                {
                    MessageBoxService.ShowError("Error opening scenario file:\n" + e.Message);
                }
            }
        }
    }
}
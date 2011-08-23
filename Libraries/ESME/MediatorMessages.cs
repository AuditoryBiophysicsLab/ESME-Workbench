using System;
using Cinch;

namespace ESME
{
    public static class MediatorMessage
    {
        public const string CancelCurrentCommand = "CancelCurrentCommandMessage";
        public const string SaveExperimentAsCommand = "SaveExperimentAsCommandMessage";
        public const string SetGridOverlayDisplay = "ToggleGridOverlayDisplayCommandMessage";
        public const string SetPanZoomDisplay = "TogglePanZoomDisplayCommandMessage";
        public const string SetScaleBarDisplay = "ToggleScaleBarDisplayCommandMessage";
        public const string SetAnalysisPointMode = "AnalysisPointCommandMessage";
        public const string QuickLookPointCommand = "QuickLookPointCommandMessage";
        public const string AddFileCommand = "AddFileCommandMessage";
        public const string AddScenarioFileCommand = "AddScenarioFileCommandMessage";
        public const string SetScenarioMapExtent = "SetScenarioMapExtentMessage";

        public const string AddAnalysisPoint = "AddAnalysisPointMessage";
        public const string EditAnalysisPoint = "EditAnalysisPointMessage";
        public const string RemoveAnalysisPoint = "DeleteAnalysisPointMessage";
        public const string ViewAnalysisPoint = "ViewAnalysisPointMessage";
        public const string CalculateAnalysisPoint = "CalculateAnalysisPointMessage";

        public const string ShowProperties = "ShowPropertiesMessage";

        public const string SetExperiment = "SetExperimentMessage";
        public const string SetMapLayers = "SetMapLayersMessage";
        public const string SetTreeRoots = "SetTreeRootsMessage";
        public const string EnableGUI = "EnableGUIMessage";

        public const string LayerListViewModelInitialized = "LayerListViewModelInitializedMessage";
        public const string MapViewModelInitialized = "MapViewModelInitializedMessage";
        public const string MainViewModelInitialized = "MainViewModelInitializedMessage";
        public const string AllViewModelsAreReady = "AllViewModelsAreReadyMessage";

        public const string AnalysisPointViewInitialized = "AnalysisPointViewInitializedMessage";
        public const string TransmissionLossFieldViewInitialized = "TransmissionLossFieldViewInitializedMessage";
        public const string TransmissionLossRadialViewInitialized = "TransmissionLossRadialViewInitializedMessage";
        public const string AnalysisPointChanged = "AnalysisPointChangedMessage";
        public const string TransmissionLossFieldChanged = "TransmissionLossFieldChangedMessage";
        public const string TransmissionLossRadialColorMapChanged = "TransmissionLossRadialColorMapChangedMessage";
        public const string TransmissionLossRadialChanged = "TransmissionLossRadialChangedMessage";
        public const string SaveRadialAsCSV = "SaveRadialAsCSVMessage";
        public const string SaveRadialBitmap = "SaveRadialBitmapMessage";
        public const string SetSelectedRadialBearing = "SetSelectedRadialBearingMessage";
        public const string SetSelectedDepth = "SetSelectedDepthMessage";
        public const string SetSelectedTransmissionLossFieldName = "SetSelectedTransmissionLossFieldNameMessage";
        public const string ResetSelectedField = "ResetSelectedFieldMessage";
        public const string AcousticOptions = "AcousticOptionsMessage";
        public const string SetTransmissionLossBathymetry = "SetTransmissionLossBathymetryMessage";
        public const string RequestTransmissionLossBathymetry = "RequestTransmissionLossBathymetryMessage";
        public const string TransmissionLossRadialEarthCoordinate = "TransmissionLossRadialEarthCoordinateMessage";
        public const string CancelCurrentTransmissionLossCalculation = "CancelCurrentTransmissionLossCalculationMessage";

        public const string SetCurrentScale = "SetCurrentScaleMessage";
        public const string GetCurrentScale = "GetCurrentScaleMessage";
        public const string CurrentScale = "CurrentScaleMessage";

        public const string SetCurrentExtent = "SetCurrentExtentMessage";
        public const string GetCurrentExtent = "GetCurrentExtentMessage";
        public const string CurrentExtent = "CurrentExtentMessage";

        public const string SetLayerIndex = "SetLayerIndexMessage";

        public const string ExperimentLoaded = "ExperimentLoadedMessage";
        public const string SetExperimentAsModified = "SetExperimentAsModifiedMessage";

        public const string DoNothing = "DoNothingMessage";
        public const string SetMapCursor = "SetMapCursorMessage";
        public const string RefreshMap = "RefreshMapMessage";
        public const string RefreshLayer = "RefreshLayerMessage";

        public const string AddListLayer = "AddListLayerMessage";
        public const string RemoveLayer = "RemoveLayerMessage";
        public const string MapLayerIndexQuery = "MapLayerIndexQueryMessage";
        public const string LayersReordered = "ListLayerMoveToIndexMessage";

        public const string MoveLayerToTop = "MoveLayerToTopMessage";
        public const string MoveLayerUp = "MoveLayerUpMessage";
        public const string MoveLayerDown = "MoveLayerDownMessage";
        public const string MoveLayerToBottom = "MoveLayerToBottomMessage";
        
        public const string ToggleLayerVisibility = "ToggleLayerVisibilityMessage";

        public const string SetupAndRunQuickLookPoint = "SetupAndRunQuickLookPointMessage";
        public const string PlaceAnalysisPoint = "SetupAndRunAnalysisPointMessage";
        public const string RunExperimentCommand = "RunExperimentCommandMessage";

        public const string SetMouseEarthCoordinate = "SetMouseEarthCoordinateMessage";

        public const string QueueTransmissionLossJob = "QueueTransmissionLossJobMessage";

        public const string CreateMMMBSBathymetryFileCommand = "CreateMMMBSBathymetryFileCommandMessage";
        public const string AddAnimatPopulationFileCommand = "AddAnimatPopulationFileCommandMessage";

        public const string DavesTestCommand = "DavesTestCommandMessage";

        public const string ApplicationClosing = "ApplicationClosingMessage";

        public const string RegisterTimePeriodSelectionViewModel = "RegisterTimePeriodSelectionViewModelMessage";

        public static void Send<T>(string key, T message) 
        {
            try
            {
                Mediator.Instance.NotifyColleagues(key, message);
            }
            catch (Exception) {}
        }
        public static void Send(string key) { Mediator.Instance.NotifyColleagues(key, true); }
        public static void SendAsync<T>(string key, T message) { Mediator.Instance.NotifyColleaguesAsync(key, message); }
        public static void SendAsync(string key) { Mediator.Instance.NotifyColleaguesAsync(key, true); }
    }
}
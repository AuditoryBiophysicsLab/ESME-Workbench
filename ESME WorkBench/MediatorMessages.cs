using System;
using Cinch;

namespace ESMEWorkBench
{
    internal static class MediatorMessage
    {
        public const string TestTransmissionLossViewCommand = "TestTransmissionLossViewCommandMessage";
        public const string CancelCurrentCommand = "CancelCurrentCommandMessage";
        public const string SaveExperimentAsCommand = "SaveExperimentAsCommandMessage";
        public const string ToggleGridOverlayDisplayCommand = "ToggleGridOverlayDisplayCommandMessage";
        public const string TogglePanZoomDisplayCommand = "TogglePanZoomDisplayCommandMessage";
        public const string ToggleScaleBarDisplayCommand = "ToggleScaleBarDisplayCommandMessage";
        public const string QuickLookCommand = "QuickLookCommandMessage";
        public const string AddFileCommand = "AddFileCommandMessage";
        public const string AddScenarioFileCommand = "AddScenarioFileCommandMessage";

        public const string AddAnalysisPoint = "AddAnalysisPointMessage";
        public const string EditAnalysisPoint = "EditAnalysisPointMessage";
        public const string DeleteAnalysisPoint = "DeleteAnalysisPointMessage";

        public const string SetExperiment = "SetExperimentMessage";

        public const string SetLayerCollection = "SetLayerCollectionMessage";
        public const string LayerAdded = "LayerAddedMessage";

        public const string LayerListViewModelInitialized = "LayerListViewModelInitializedMessage";
        public const string MapViewModelInitialized = "MapViewModelInitializedMessage";
        public const string MainViewModelInitialized = "MainViewModelInitializedMessage";

        public const string TransmissionLossFieldViewInitialized = "TransmissionLossFieldViewInitializedMessage";
        public const string TransmissionLossRadialViewInitialized = "TransmissionLossRadialViewInitializedMessage";
        public const string TransmissionLossRadialColorMapChanged = "TransmissionLossRadialColorMapChangedMessage";
        public const string TransmissionLossRadialChanged = "TransmissionLossRadialChangedMessage";

        public const string CurrentScaleChanged = "CurrentScaleChangedMessage";
        public const string CurrentExtentChanged = "CurrentExtentChangedMessage";
        public const string SetCurrentScale = "SetCurrentScaleMessage";
        public const string SetCurrentExtent = "SetCurrentExtentMessage";

        public const string SetLayerIndex = "SetLayerIndexMessage";

        public const string ExperimentClosed = "ExperimentClosedMessage";
        public const string ExperimentLoaded = "ExperimentLoadedMessage";
        public const string SetExperimentAsModified = "SetExperimentAsModifiedMessage";

        public const string DoNothing = "DoNothingMessage";
        public const string SetMapCursor = "SetMapCursorMessage";
        public const string RefreshMap = "RefreshMapMessage";
        public const string RefreshLayer = "RefreshLayerMessage";

        public const string AddMapLayer = "AddMapLayerMessage";
        public const string AddListLayer = "AddListLayerMessage";
        public const string RemoveLayer = "RemoveLayerMessage";
        public const string MapLayerIndexQuery = "MapLayerIndexQueryMessage";
        public const string LayersReordered = "ListLayerMoveToIndexMessage";

        public const string MoveLayerToTop = "MoveLayerToTopMessage";
        public const string MoveLayerUp = "MoveLayerUpMessage";
        public const string MoveLayerDown = "MoveLayerDownMessage";
        public const string MoveLayerToBottom = "MoveLayerToBottomMessage";
        
        public const string ToggleLayerVisibility = "ToggleLayerVisibilityMessage";
        
        public const string RunQuickLook = "RunQuickLookMessage";

        public const string SetMouseEarthCoordinate = "SetMouseEarthCoordinateMessage";

        public const string LaunchMMMBSCommand = "LaunchMMMBSCommandMessage";
        public const string CreateMMMBSBathymetryFileCommand = "CreateMMMBSBathymetryFileCommandMessage";
        public const string AddAnimatPopulationFileCommand = "AddAnimatPopulationFileCommandMessage";

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
using System;
using HRC.ViewModels;

namespace ESME
{
    public static class MediatorMessage
    {
        public const string ApplicationClosing = "ApplicationClosingMessage";
        public const string CancelCurrentCommand = "CancelCurrentCommandMessage";
        public const string SetAnalysisPointMode = "AnalysisPointCommandMessage";

        public const string AddAnalysisPoint = "AddAnalysisPointMessage";
        public const string EditAnalysisPoint = "EditAnalysisPointMessage";
        public const string DeleteAnalysisPoint = "DeleteAnalysisPointMessage";
        public const string ViewAnalysisPoint = "ViewAnalysisPointMessage";
        public const string CalculateAnalysisPoint = "CalculateAnalysisPointMessage";

        public const string ScenarioBoundToLayer = "ScenarioBoundToLayerMessage";

        public const string AddPlatform = "AddPlatformMessage";
        public const string DeletePlatform = "DeletePlatformMessage";
        public const string PlatformProperties = "PlatformPropertiesMessage";
        public const string PlatformBoundToLayer = "PlatformBoundToLayerMessage";

        public const string AddSource = "AddSourceMessage";
        public const string DeleteSource = "DeleteSourceMessage";
        public const string SourceProperties = "SourcePropertiesMessage";
        public const string SourceBoundToLayer = "SourceBoundToLayerMessage";

        public const string AddMode = "AddModeMessage";
        public const string DeleteMode = "DeleteModeMessage";
        public const string ModeProperties = "ModePropertiesMessage";
        public const string ModeBoundToLayer = "ModeBoundToLayerMessage";

        public const string ShowProperties = "ShowPropertiesMessage";

        public const string SetExperiment = "SetExperimentMessage";
        public const string SetMapLayers = "SetMapLayersMessage";
        public const string SetTreeRoots = "SetTreeRootsMessage";
        
        public const string ViewTransmissionLoss = "ViewTransmissionLossMessage";
        public const string DeleteTransmissionLoss = "DeleteTransmissionLossMessage";
        public const string TransmissionLossLayerChanged = "TransmissionLossLayerChangedMessage";

        public const string ShowTransmissionLossQueueView = "ShowTransmissionLossQueueViewMessage";

        public const string SetMapExtent = "SetMapExtentMessage";
        public const string SetMapCursor = "SetMapCursorMessage";
        public const string RefreshMap = "RefreshMapMessage";
        public const string RefreshMapLayer = "RefreshMapLayerMessage";
        public const string AddMapLayer = "AddMapLayerMessage";
        public const string RemoveMapLayer = "RemoveMapLayerMessage";
        public const string ShowMapLayer = "ShowMapLayerMessage";
        public const string HideMapLayer = "HideMapLayerMessage";

        public const string MoveLayerToTop = "MoveLayerToTopMessage";
        public const string MoveLayerUp = "MoveLayerUpMessage";
        public const string MoveLayerDown = "MoveLayerDownMessage";
        public const string MoveLayerToBottom = "MoveLayerToBottomMessage";
        
        public const string ToggleLayerVisibility = "ToggleLayerVisibilityMessage";

        public const string SetupAndRunQuickLookPoint = "SetupAndRunQuickLookPointMessage";
        public const string PlaceAnalysisPoint = "SetupAndRunAnalysisPointMessage";
        public const string RunExperimentCommand = "RunExperimentCommandMessage";

        public const string SetMouseEarthCoordinate = "SetMouseEarthCoordinateMessage";
        public const string MapLeftButtonDown = "MapLeftButtonDownMessage";
        public const string MapLeftButtonUp = "MapLeftButtonUpMessage";
        public const string MapRightButtonDown = "MapRightButtonDownMessage";
        public const string MapRightButtonUp = "MapRightButtonUpMessage";
        public const string MapClick = "MapClickMessage";
        public const string MapDoubleClick = "MapDoubleClickMessage";

        public const string QueueTransmissionLossJob = "QueueTransmissionLossJobMessage";

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
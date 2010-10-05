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
        public const string QuickLookCommand = "QuickLookCommandMessage";
        public const string AddOverlayFileCommand = "AddOverlayFileCommandMessage";
        public const string AddShapefileCommand = "AddShapefileCommandMessage";
        public const string AddScenarioFileCommand = "AddScenarioFileCommandMessage";

        public const string AddScenarioFile = "AddScenarioFileMessage";
        public const string AddShapefile = "AddShapefileMessage";
        public const string AddOverlayFile = "AddOverlayFileMessage";

        public const string LayerListViewModelInitialized = "LayerListViewModelInitializedMessage";
        public const string MapViewModelInitialized = "MapViewModelInitializedMessage";
        public const string MainViewModelInitialized = "MainViewModelInitializedMessage";

        public const string TransmissionLossFieldViewInitialized = "TransmissionLossFieldViewInitializedMessage";
        public const string TransmissionLossRadialViewInitialized = "TransmissionLossRadialViewInitializedMessage";

        public const string ExperimentLoaded = "ExperimentLoadedMessage";

        public const string InitializeMapView = "InitializeMapViewMessage";
        public const string SetMapCursor = "SetMapCursorMessage";
        public const string RefreshMap = "RefreshMapMessage";

        public const string AddMapLayer = "AddMapLayerMessage";
        public const string AddListLayer = "AddListLayerMessage";
        public const string RemoveLayer = "RemoveLayerMessage";
        public const string MapLayerIndexQuery = "MapLayerIndexQueryMessage";
        public const string ListLayerMoveToIndex = "ListLayerMoveToIndexMessage";
        public const string SetLayerLineColor = "SetLayerLineColorMessage";
        public const string SetLayerAreaColor = "SetLayerAreaColorMessage";

        public const string MoveLayerToTop = "MoveLayerToTopMessage";
        public const string MoveLayerUp = "MoveLayerUpMessage";
        public const string MoveLayerDown = "MoveLayerDownMessage";
        public const string MoveLayerToBottom = "MoveLayerToBottomMessage";
        
        public const string ToggleLayerVisibility = "ToggleLayerVisibilityMessage";
        
        public const string ChangeLayerLineColor = "ChangeLayerLineColorMessage";
        public const string ChangeLayerLineWidth = "ChangeLayerLineWidthMessage";
        public const string ChangeLayerFillColor = "ChangeLayerFillColorMessage";

        public const string RunQuickLook = "RunQuickLookMessage";

        public const string SetMouseEarthCoordinate = "SetMouseEarthCoordinateMessage";

        public static void Send<T>(string key, T message) { Mediator.Instance.NotifyColleagues(key, message); }
        public static void Send(string key) { Mediator.Instance.NotifyColleagues(key, true); }
        public static void SendAsync<T>(string key, T message) { Mediator.Instance.NotifyColleaguesAsync(key, message); }
        public static void SendAsync(string key) { Mediator.Instance.NotifyColleaguesAsync(key, true); }
    }
}
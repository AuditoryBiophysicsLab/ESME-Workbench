using System;
using HRC.ViewModels;

namespace ESME
{
    public static class MediatorMessage
    {
        public const string ApplicationClosing = "ApplicationClosingMessage";
        public const string SetAnalysisPointMode = "AnalysisPointCommandMessage";

        public const string AddAnalysisPoint = "AddAnalysisPointMessage";
        public const string EditAnalysisPoint = "EditAnalysisPointMessage";
        public const string DeleteAnalysisPoint = "DeleteAnalysisPointMessage";
        public const string ViewAnalysisPoint = "ViewAnalysisPointMessage";
        public const string ViewAnalysisPointProperties = "ViewAnalysisPointPropertiesMessage";
        public const string CalculateAnalysisPoint = "CalculateAnalysisPointMessage";
        public const string DeleteAllAnalysisPoints = "DeleteAllAnalysisPointsMessage";
        public const string RecalculateAllAnalysisPoints = "RecalculateAllAnalysisPointsMessage";
        public const string RecalculateAnalysisPoint = "RecalculateAnalysisPointMessage";

        public const string CreateLocation = "CreateLocationMessage";
        public const string DeleteLocation = "DeleteLocationMessage";

        public const string CreateScenario = "CreateScenarioMessage";
        public const string DeleteScenario = "DeleteScenarioMessage";
        public const string LoadScenario = "LoadScenarioMessage";
        public const string DeleteAllScenarios = "DeleteAllScenariosMessage";
        public const string ScenarioProperties = "ScenarioPropertiesMessage";
        public const string SaveScenarioCopy = "SaveScenarioCopyMessage";

        public const string ScenarioBoundToLayer = "ScenarioBoundToLayerMessage";
        public const string ViewScenarioProperties = "ViewScenarioPropertiesMessage";

        public const string AddPlatform = "AddPlatformMessage";
        public const string EditPSMPlatform = "EditPSMPlatformMessage";
        
        public const string PSMPlatformChanged = "PSMPlatformChangedMessage";
        public const string CopyPSMPlatform = "CopyPSMPlatformMessage";
        public const string DeletePSMPlatform = "DeletePSMPlatformMessage";
        public const string DeletePlatform = "DeletePlatformMessage";
        public const string PlatformProperties = "PlatformPropertiesMessage";
        public const string PlatformBoundToLayer = "PlatformBoundToLayerMessage";
        public const string PastePSMSource = "PastePSMSourceMessage";

        public const string AddSource = "AddSourceMessage";
        public const string AddPSMSource = "AddPSMSourceMessage";
        public const string PSMSourceChanged = "PSMSourceChangedMessage";
        public const string PSMSourceAdded = "PSMSourceAddedMessage";
        public const string EditPSMSource = "EditPSMSourceMessage";
        public const string CopyPSMSource = "CopyPSMSourceMessage";
        public const string DeleteSource = "DeleteSourceMessage";
        public const string DeletePSMSource = "DeletePSMSourceMessage";
        public const string SourceProperties = "SourcePropertiesMessage";
        public const string SourceBoundToLayer = "SourceBoundToLayerMessage";

        public const string AddMode = "AddModeMessage";
        public const string AddPSMMode = "AddPSMModeMessage";
        public const string PSMModeChanged = "PSMModeChangedMessage";
        public const string EditPSMMode = "EditPSMModeMessage";
        public const string CopyPSMMode = "CopyPSMModeMessage";
        public const string DeleteMode = "DeleteModeMessage";
        public const string DeletePSMMode = "DeletePSMModeMessage";
        public const string RecalculateMode = "RecalculateModeMessage";
        public const string ModeProperties = "ModePropertiesMessage";
        public const string ModeBoundToLayer = "ModeBoundToLayerMessage";

        public const string AddPerimeter = "AddPerimeterMessage";
        public const string DeletePerimeter = "DeletePerimeterMessage";
        public const string EditPerimeter = "EditPerimeterMessage";
        public const string PerimeterProperties = "PerimeterPropertiesMessage";

        public const string AddSpecies = "AddSpeciesMessage";
        public const string DeleteAllSpecies = "DeleteAllSpeciesMessage";
        public const string RepopulateAllSpecies = "RepopulateAllSpeciesMessage";
        public const string DeleteSpecies = "DeleteSpeciesMessage";
        public const string RepopulateSpecies = "RepopulateSpeciesMessage";
        public const string SpeciesProperties = "SpeciesPropertiesMessage";

        public const string ShowProperties = "ShowPropertiesMessage";

        public const string SetExperiment = "SetExperimentMessage";
        public const string SetMapLayers = "SetMapLayersMessage";
        public const string SetTreeRoots = "SetTreeRootsMessage";
        
        public const string ViewTransmissionLoss = "ViewTransmissionLossMessage";
        public const string DeleteTransmissionLoss = "DeleteTransmissionLossMessage";
        public const string RecalculateTransmissionLoss = "RecalculateTransmissionLossMessage";
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

        public const string MoveLayerToFront = "MoveLayerToFrontMessage";
        public const string MoveLayerForward = "MoveLayerForwardMessage";
        public const string MoveLayerBackward = "MoveLayerBackwardMessage";
        public const string MoveLayerToBack = "MoveLayerToBackMessage";
        
        public const string ToggleLayerVisibility = "ToggleLayerVisibilityMessage";

        public const string SetupAndRunQuickLookPoint = "SetupAndRunQuickLookPointMessage";
        public const string RunExperimentCommand = "RunExperimentCommandMessage";

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
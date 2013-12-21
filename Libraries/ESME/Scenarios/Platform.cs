using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq; 
using System.Windows.Input;
using System.Windows.Media;
using ESME.Behaviors;
using ESME.Database;
using ESME.Locations;
using ESME.Mapping;
using HRC;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Services;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Scenarios
{
    [NotifyPropertyChanged]
    public class Platform : IHaveGuid, IHaveLayerSettings
    {
        #region Mapped Properties
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string Description { get; set; }
        public bool Launches { get; set; }
        public bool Tows { get; set; }
        public int RepeatCount { get; set; }
        // Copied from the PSM Platform
        public string PSMPlatformGuid { get; set; }
        public string PlatformName { get; set; }
        public string PlatformType { get; set; }

        public DbTrackType TrackType { get; set; }
        public DbGeo Geo { get; set; }

        public bool IsRandom { get; set; }
        public float Depth { get; set; }
        public float Course { get; set; }

        /// <summary>
        ///   Speed in knots (nautical miles per hour)
        /// </summary>
        [Affects("TrackTypeDisplay")]
        public float Speed { get; set; }

        public virtual Scenario Scenario { get; set; }

        [Affects("TrackTypeDisplay")]
        public virtual Perimeter Perimeter { get; set; }

        public virtual ShipTrack ShipTrack { get; set; }

        [Initialize] public virtual LayerSettings LayerSettings { get; set; }

        [Initialize] public virtual ObservableList<Source> Sources { get; set; }

        [Initialize] public virtual ObservableList<LogEntry> Logs { get; set; }
        #endregion

        #region Unmapped Properties
         
        PlatformBehavior _platformBehavior;
        [NotMapped]
        public PlatformBehavior PlatformBehavior
        {
            get
            {
                if (_platformBehavior != null && Scenario != null) return _platformBehavior;
                if (Scenario == null) return null;
                _platformBehavior = new PlatformBehavior(this, new TimeSpan(0, 0, 1, 0), (int)((TimeSpan)Scenario.Duration).TotalMinutes);
                return _platformBehavior;
            }
            set { _platformBehavior = value; }
        }

        [NotMapped]
        public List<TrackType> TrackTypeDisplay
        {
            get { return Perimeter == null || Speed == 0 ? StationaryOnly : AllTrackTypes; }
        }

        [NotMapped]
        public TrackType SelectedTrackType
        {
            get { return TrackType; }
            set
            {
                TrackType oldValue = TrackType;
                try
                {
                    TrackType = value;
                }
                catch (Exception e)
                {
                    if (_messageBox != null) _messageBox.ShowError(e.Message);
                    TrackType = oldValue;
                }
                finally
                {
                    Refresh();
                }
            }
        }

        [NotMapped]
        public int ActorID { get; set; }

        [NotMapped]
        public bool IsNew { get; set; }

        [NotMapped]
        public bool IsDeleted { get; set; }
         
        object _layerControl;
        [NotMapped]
        public object LayerControl
        {
            get { return _layerControl; }
            set
            {
                _layerControl = value;
                MediatorMessage.Send(MediatorMessage.PlatformBoundToLayer, this);
            }
        }
        #endregion

        #region Private members

        [Import, UsedImplicitly] static IMessageBoxService _messageBox;

        static readonly List<TrackType> StationaryOnly = new List<TrackType> { Behaviors.TrackType.Stationary, Behaviors.TrackType.WaypointFile };
         
        static readonly List<TrackType> AllTrackTypes = new List<TrackType> { Behaviors.TrackType.Stationary, Behaviors.TrackType.PerimeterBounce, Behaviors.TrackType.WaypointFile };

        #endregion

        public Platform() { Initialize(); }

        void Initialize()
        {
            TrackType = Behaviors.TrackType.Stationary;
            ((INotifyPropertyChanged)this).PropertyChanged += NotifyPropertyChanged;
        }

        void NotifyPropertyChanged(object sender, PropertyChangedEventArgs args)
        {
            switch (args.PropertyName)
            {
                case "IsRandom":
                case "Perimeter":
                case "SelectedTrackType":
                case "Geo":
                case "ShipTrack":
                    Refresh();
                    break;
            }
        }

        public Platform(Platform platform)
        {
            Copy(platform);
            ((INotifyPropertyChanged)this).PropertyChanged += NotifyPropertyChanged;
        }

        void Copy(Platform platform)
        {
            Description = platform.Description;
            Launches = platform.Launches;
            Tows = platform.Tows;
            RepeatCount = platform.RepeatCount;
            PSMPlatformGuid = platform.PSMPlatformGuid;
            PlatformName = platform.PlatformName;
            PlatformType = platform.PlatformType;
            TrackType = (TrackType)platform.TrackType;
            Geo = new Geo(platform.Geo);
            IsRandom = platform.IsRandom;
            Depth = platform.Depth;
            Course = platform.Course;
            Speed = platform.Speed;
            LayerSettings = new LayerSettings(platform.LayerSettings);
            if (platform.Sources != null) foreach (var newsource in platform.Sources.Select(source => new Source(source))) Sources.Add(newsource);
            if (platform.ShipTrack != null) ShipTrack = new ShipTrack(this, platform.ShipTrack);
        }

        public static Platform NewPSMPlatform()
        {
            var platform = new Platform
            {
                Description = "New Platform",
                Launches = false,
                Tows = false,
                RepeatCount = 0,
                PlatformName = "New Platform",
                PlatformType = "New",
                TrackType = new DbTrackType(Behaviors.TrackType.Stationary),
                Geo = new DbGeo(),
                IsRandom = false,
                Depth = 0,
                Course = 0,
                Speed = 0,
            };
            platform.ShipTrack.Platform = platform;
            return platform;
        }

        public void Delete()
        {
            RemoveMapLayers();
            Scenario.Platforms.Remove(this);
            foreach (var source in Sources.ToList()) source.Delete();
            if (LayerSettings != null) Globals.MasterDatabaseService.Context.LayerSettings.Remove(LayerSettings);
            LayerSettings = null;
            Globals.MasterDatabaseService.Context.Platforms.Remove(this);
        }

        public void Refresh()
        {
            RemoveMapLayers();
            PlatformBehavior = null;
            UpdateMapLayers();
        }

        #region Map Layer methods

        public void UpdateMapLayers()
        {
            if (Scenario == null || LayerSettings == null) return;
            if (!Scenario.IsLoaded) return;
            var mapLayer = (LayerSettings.MapLayerViewModel != null) ? (OverlayShapeMapLayer)LayerSettings.MapLayerViewModel : new OverlayShapeMapLayer { Name = string.Format("{0}", Guid) };
            mapLayer.CustomLineStyleFunc = m => new CustomStartEndLineStyle("Wingdings", 0x6C, Colors.Green, m.LineWidth + 8, "Wingdings", 0x6E, Colors.Red, m.LineWidth + 8, m.LineColor, m.LineWidth);
            List<Geo> locations;
            try
            {
                locations = PlatformBehavior.PlatformStates.Select(p => p.PlatformLocation.Location).ToList();
            }
            catch (NullReferenceException)
            {
                return;
            }

            try
            {
                mapLayer.AddLines(locations);
            }
            catch (Exception)
            {
#if false
                var log = LogManager.GetLogger(GetType());
                log.Error(string.Format("AddLines failed:{0}", e.Message));
                log.ErrorFormat("Malformed wellformedtext:{0}", OverlayShapeMapLayer.WellKnownText("LINESTRING(", locations, ")"));
                log.Error(string.Format("locations.count:{0}", locations.Count));
                Scenario.Log();
#endif
            }
            mapLayer.Done();
            LayerSettings.MapLayerViewModel = mapLayer;
        }
        public void RemoveMapLayers() { if (LayerSettings != null) LayerSettings.MapLayerViewModel = null; }
        #endregion
        
        #region Commands
        #region PlatformPropertiesCommand
        public SimpleCommand<object, EventToCommandArgs> PlatformPropertiesCommand
        {
            get
            {
                return _platformProperties ??
                       (_platformProperties = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.PlatformProperties, this)));
            }
        }
         
        SimpleCommand<object, EventToCommandArgs> _platformProperties;
        #endregion

        #region DeletePlatformCommand
        public SimpleCommand<object, EventToCommandArgs> DeletePlatformCommand
        {
            get { return _deletePlatform ?? (_deletePlatform = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeletePlatform, this))); }
        }
         
        SimpleCommand<object, EventToCommandArgs> _deletePlatform;
        #endregion

        #region AddSourceCommand
        public SimpleCommand<object, EventToCommandArgs> AddSourceCommand
        {
            get { return _addSource ?? (_addSource = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.AddSource, this))); }
        }
         
        SimpleCommand<object, EventToCommandArgs> _addSource;
        #endregion

        #region KeyUpCommand
        public SimpleCommand<object, EventToCommandArgs> KeyUpCommand
        {
            get { return _keyUp ?? (_keyUp = new SimpleCommand<object, EventToCommandArgs>(KeyUpHandler)); }
        }
         
        SimpleCommand<object, EventToCommandArgs> _keyUp;

        void KeyUpHandler(EventToCommandArgs args)
        {
            var parameter = (KeyEventArgs)args.EventArgs;
            switch (parameter.Key)
            {
                case Key.Delete:
                    MediatorMessage.Send(MediatorMessage.DeletePlatform, this);
                    break;
                case Key.Insert:
                    MediatorMessage.Send(MediatorMessage.AddSource, this);
                    break;
                default:
                    return;
            }
        }
        #endregion

        #region PSM database commands

        #region AddPSMSourceCommand
        public SimpleCommand<object, EventToCommandArgs> AddPSMSourceCommand
        {
            get
            {
                return _addPSMSource ?? (_addPSMSource = new SimpleCommand<object, EventToCommandArgs>(o =>
                {
                    var source = Source.NewPSMSource(this);
                    MediatorMessage.Send(MediatorMessage.AddPSMSource, source);
                }));
            }
        }
         
        SimpleCommand<object, EventToCommandArgs> _addPSMSource;
        #endregion

        [MediatorMessageSink(MediatorMessage.CopyPSMSource),UsedImplicitly]
        void EnablePaste(bool dummy) { IsPastePSMSourceCommandEnabled = true; }

        #region PastePSMSourceCommand
        public SimpleCommand<object, EventToCommandArgs> PastePSMSourceCommand
        {
            get { return _pastePSMSource ?? (_pastePSMSource = new SimpleCommand<object, EventToCommandArgs>(o => IsPastePSMSourceCommandEnabled, PastePSMSourceHandler)); }
        }
         
        SimpleCommand<object, EventToCommandArgs> _pastePSMSource;

        bool IsPastePSMSourceCommandEnabled { get; set; }

        void PastePSMSourceHandler(EventToCommandArgs args)
        {
            MediatorMessage.Send(MediatorMessage.PastePSMSource,this);
        }
        #endregion

        #region EditPSMPlatformCommand
        public SimpleCommand<object, EventToCommandArgs> EditPSMPlatformCommand
        {
            get { return _editPSMPlatform ?? (_editPSMPlatform = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.EditPSMPlatform, this))); }
        }
         
        SimpleCommand<object, EventToCommandArgs> _editPSMPlatform;
        #endregion

        #region DeletePSMPlatformCommand
        public SimpleCommand<object, EventToCommandArgs> DeletePSMPlatformCommand
        {
            get { return _deletePSMPlatform ?? (_deletePSMPlatform = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeletePSMPlatform,this))); }
        }
         
        SimpleCommand<object, EventToCommandArgs> _deletePSMPlatform;

        #endregion

        #region CopyPSMPlatformCommand
        public SimpleCommand<object, EventToCommandArgs> CopyPSMPlatformCommand
        {
            get { return _copyPSMPlatform ?? (_copyPSMPlatform = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.CopyPSMPlatform, this))); }
        }
         
        SimpleCommand<object, EventToCommandArgs> _copyPSMPlatform;

        #endregion

        #endregion

        #region Layer Move commands

        #region MoveLayerToFrontCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerToFrontCommand
        {
            get
            {
                return _moveLayerToFront ?? (_moveLayerToFront = new SimpleCommand<object, EventToCommandArgs>(o =>
                {
                    LayerSettings.MoveLayerToFront();
                    MediatorMessage.Send(MediatorMessage.RefreshMap, true);
                }));
            }
        }
         
        SimpleCommand<object, EventToCommandArgs> _moveLayerToFront;
        #endregion

        #region MoveLayerForwardCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerForwardCommand
        {
            get
            {
                return _moveLayerForward ?? (_moveLayerForward = new SimpleCommand<object, EventToCommandArgs>(o =>
                {
                    LayerSettings.MoveLayerForward();
                    MediatorMessage.Send(MediatorMessage.RefreshMap, true);
                }));
            }
        }
         
        SimpleCommand<object, EventToCommandArgs> _moveLayerForward;
        #endregion

        #region MoveLayerBackwardCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerBackwardCommand
        {
            get
            {
                return _moveLayerBackward ?? (_moveLayerBackward = new SimpleCommand<object, EventToCommandArgs>(o =>
                {
                    LayerSettings.MoveLayerBackward();
                    MediatorMessage.Send(MediatorMessage.RefreshMap, true);
                }));
            }
        }
         
        SimpleCommand<object, EventToCommandArgs> _moveLayerBackward;
        #endregion

        #region MoveLayerToBackCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerToBackCommand
        {
            get
            {
                return _moveLayerToBack ?? (_moveLayerToBack = new SimpleCommand<object, EventToCommandArgs>(o =>
                {
                    LayerSettings.MoveLayerToBack();
                    MediatorMessage.Send(MediatorMessage.RefreshMap, true);
                }));
            }
        }
         
        SimpleCommand<object, EventToCommandArgs> _moveLayerToBack;
        #endregion

        #endregion
        #endregion
    }
}
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Threading;
using ESME.Behaviors;
using ESME.Database;
using ESME.Locations;
using ESME.Mapping;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Services;
using HRC.Utility;  
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Scenarios
{
    [NotifyPropertyChanged]
    public class Platform : IHaveGuid, IHaveLayerSettings, INotifyPropertyChanged
    {
        public Platform() { TrackType = Behaviors.TrackType.Stationary; }
        public Platform(Platform platform) { Copy(platform); }
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
        }

        [Import] static readonly IMessageBoxService _messageBox;

        [Key, Initialize] public Guid Guid { get; set; }
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
        bool _isRandom = true;

        public bool IsRandom
        {
            get { return _isRandom; }
            set
            {
                _isRandom = value;
                if (LayerSettings == null || LayerSettings.MapLayerViewModel == null) return;
                RemoveMapLayers();
                PlatformBehavior = null;
                CreateMapLayers();
            }
        }

        public float Depth { get; set; }
        public float Course { get; set; }

        /// <summary>
        ///   Speed in knots (nautical miles per hour)
        /// </summary>
        [Affects("TrackTypeDisplay")]
        public float Speed { get; set; }

        public virtual Scenario Scenario { get; set; }

        Perimeter _perimeter;

        [Affects("TrackTypeDisplay")]
        public virtual Perimeter Perimeter
        {
            get { return _perimeter; }
            set
            {
                _perimeter = value;
                if (LayerSettings == null || LayerSettings.MapLayerViewModel == null) return;
                RemoveMapLayers();
                PlatformBehavior = null;
                CreateMapLayers();
            }
        }

        [Initialize] public virtual LayerSettings LayerSettings { get; set; }
        [Initialize] public virtual ObservableList<Source> Sources { get; set; }
        [Initialize] public virtual ObservableList<LogEntry> Logs { get; set; }
        [NotMapped] public int ActorID { get; set; }
        [NotMapped] public bool IsNew { get; set; }
        [NotMapped] public bool IsDeleted { get; set; }

        [NotMapped] public object LayerControl
        {
            get { return _layerControl; }
            set
            {
                _layerControl = value;
                MediatorMessage.Send(MediatorMessage.PlatformBoundToLayer, this);
            }
        }

        object _layerControl;

        static readonly List<TrackType> StationaryOnly = new List<TrackType> { Behaviors.TrackType.Stationary };
        static readonly List<TrackType> AllTrackTypes = new List<TrackType> { Behaviors.TrackType.Stationary, Behaviors.TrackType.PerimeterBounce };
        [NotMapped] public List<TrackType> TrackTypeDisplay { get { return Perimeter == null || Speed == 0 ? StationaryOnly : AllTrackTypes; } }

        [NotMapped] public TrackType SelectedTrackType
        {
            get { return TrackType; }
            set
            {
                TrackType oldValue = TrackType;
                try
                {
                    TrackType = value;
                    if (LayerSettings == null || LayerSettings.MapLayerViewModel == null) return;
                    RemoveMapLayers();
                    PlatformBehavior = null;
                    CreateMapLayers();
                }
                catch (Exception e)
                {
                    if (_messageBox != null) _messageBox.ShowError(e.Message);
                    TrackType = oldValue;
                    OnPropertyChanged("SelectedTrackType");
                }
            }
        }

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
        public SimpleCommand<object, EventToCommandArgs> DeletePlatformCommand { get { return _deletePlatform ?? (_deletePlatform = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeletePlatform, this))); } }
        SimpleCommand<object, EventToCommandArgs> _deletePlatform;
        #endregion

        public void Delete()
        {
            RemoveMapLayers();
            Scenario.Platforms.Remove(this);
            foreach (var source in Sources.ToList()) source.Delete();
            if (LayerSettings != null) Scenario.Database.Context.LayerSettings.Remove(LayerSettings);
            LayerSettings = null;
            Scenario.Database.Context.Platforms.Remove(this);
        }

        #region AddSourceCommand
        public SimpleCommand<object, EventToCommandArgs> AddSourceCommand { get { return _addSource ?? (_addSource = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.AddSource, this))); } }
        SimpleCommand<object, EventToCommandArgs> _addSource;
        #endregion

        #region KeyUpCommand
        public SimpleCommand<object, EventToCommandArgs> KeyUpCommand { get { return _keyUp ?? (_keyUp = new SimpleCommand<object, EventToCommandArgs>(KeyUpHandler)); } }
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

        #region AddPSMSourceCommand
        public SimpleCommand<object, EventToCommandArgs> AddPSMSourceCommand
        {
            get { return _addPSMSource ?? (_addPSMSource = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.AddPSMSource))); }
        }

        SimpleCommand<object, EventToCommandArgs> _addPSMSource;

        #endregion

        PlatformBehavior _platformBehavior;

        [NotMapped] public PlatformBehavior PlatformBehavior
        {
            get
            {
                if (_platformBehavior != null && Scenario !=null) return _platformBehavior;
                _platformBehavior = new PlatformBehavior(this, new TimeSpan(0, 0, 1, 0), (int)((TimeSpan)Scenario.Duration).TotalMinutes);
                return _platformBehavior;
            }
            set { _platformBehavior = value; }
        }

        public void CreateMapLayers()
        {
            var mapLayer = new OverlayShapeMapLayer
            {
                Name = string.Format("{0}", Guid),
#if true
                CustomLineStyleFunc = m => new CustomStartEndLineStyle("Wingdings", 0x6C,
                                                               Colors.Green,
                                                               m.LineWidth + 8,
                                                               "Wingdings", 0x6E,
                                                               Colors.Red,
                                                               m.LineWidth + 8,
                                                               m.LineColor,
                                                               m.LineWidth),

#else
		                CustomLineStyleFunc = m => new CustomStartEndLineStyle(PointSymbolType.Circle,
                                                                       Colors.Green,
                                                                       (int)Math.Round(m.LineWidth + 5),
                                                                       PointSymbolType.Square,
                                                                       Colors.Red,
                                                                       (int)Math.Round(m.LineWidth + 5),
                                                                       m.LineColor,
                                                                       m.LineWidth),
  
#endif
            };
            var locations = PlatformBehavior.PlatformStates.Select(p => p.PlatformLocation.Location).ToList();
            
            try
            {
                mapLayer.AddLines(locations);
            }
            catch (Exception e)
            {
                var log = log4net.LogManager.GetLogger(GetType());
                log.Error(string.Format("AddLines failed:{0}", e.Message));
                log.ErrorFormat("Malformed wellformedtext:{0}", OverlayShapeMapLayer.WellKnownText("LINESTRING(", locations, ")"));
                log.Error(string.Format("locations.count:{0}", locations.Count));
                Scenario.Log();
            }
            mapLayer.Done();
            LayerSettings.MapLayerViewModel = mapLayer;
        }

        public void Refresh()
        {
            if (LayerSettings.MapLayerViewModel == null) return;
            PlatformBehavior = null;
            RemoveMapLayers();
            CreateMapLayers();
        }

        public void RemoveMapLayers() { LayerSettings.MapLayerViewModel = null; }

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

        public event PropertyChangedEventHandler PropertyChanged;

        protected void OnPropertyChanged(string propertyName)
        {
            var handlers = PropertyChanged;
            if (handlers == null) return;
            foreach (PropertyChangedEventHandler handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                {
                    var localHandler = handler;
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => localHandler(this, new PropertyChangedEventArgs(propertyName)));
                }
                else handler(this, new PropertyChangedEventArgs(propertyName));
            }
        }
    }
}
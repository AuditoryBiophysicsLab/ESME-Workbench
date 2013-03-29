using System;
using System.ComponentModel;
using System.Windows;
using ESME.Behaviors;
using ESME.Database;
using ESME.Locations;
using ESME.Scenarios;
using HRC;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Services;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Scenarios
{
    /// <summary>
    /// To create and show the view as a dialog:
    /// var vm = new CreatePlatformViewModel {...};
    /// var result = _visualizerService.ShowDialog("CreatePlatformView", vm);
    /// if ((!result.HasValue) || (!result.Value)) return;
    /// 
    /// To create and show the view as a window:
    /// var vm = new CreatePlatformViewModel {...};
    /// var window = _visualizerService.ShowWindow("CreatePlatformView", vm);
    /// </summary>

    public class PropertiesViewModel : ViewModelBase
    {
        public string WindowTitle { get; set; }
        public object PropertyObject { get; set; }
        public bool IsPSMView { get; set; }
        
        #region OkCommand
        public SimpleCommand<object, EventToCommandArgs> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, EventToCommandArgs>(OkHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _ok;

        void OkHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter;
            if (!IsPSMView)
            {
                CloseDialog(true);
            }
            else
            {
                if (PropertyObject is Platform)
                {
                    MediatorMessage.Send(MediatorMessage.PSMPlatformChanged, PropertyObject);
                }
                if (PropertyObject is Source)
                {
                    MediatorMessage.Send(MediatorMessage.PSMSourceChanged, PropertyObject);
                }
            }
        }
        #endregion
    }

    public class PlatformPropertiesViewModel : ViewModelBase
    {
        [UsedImplicitly] PropertyObserver<Platform> _platformObserver;
        [UsedImplicitly] PropertyObserver<PlatformPropertiesViewModel> _viewModelObserver;
        public PlatformPropertiesViewModel(Platform platform, IHRCOpenFileService openFileService, IMessageBoxService messageBoxService, IMasterDatabaseService databaseService)
        {
            Platform = platform;
            Latitude = platform.Geo.Latitude;
            Longitude = platform.Geo.Longitude;
            _openFileService = openFileService;
            _messageBoxService = messageBoxService;
            _databaseService = databaseService;
            _platformObserver = new PropertyObserver<Platform>(Platform)
                .RegisterHandler(p => p.SelectedTrackType, SelectedTrackTypeChanged)
                .RegisterHandler(p => p.PlatformName, WindowTitleChanged)
                .RegisterHandler(p => p.ShipTrack, ShipTrackChanged);
            _viewModelObserver = new PropertyObserver<PlatformPropertiesViewModel>(this)
                .RegisterHandler(p => p.IsPSMView, RandomizeSectionVisibilityChanged)
                .RegisterHandler(p => p.Latitude, () => { _isGeoChanged = true; })
                .RegisterHandler(p => p.Longitude, () => { _isGeoChanged = true; });
            WindowTitleChanged();
            SelectedTrackTypeChanged();
            RandomizeSectionVisibilityChanged();
            ShipTrackChanged();
            CheckIsSpeedEnabled();
        }

        public bool IsSpeedEnabled { get; set; }
        public bool IsPSMView { get; set; }
        public Platform Platform { get; set; }
        public string WindowTitle { get; set; }
        public Visibility ImportWaypointFileVisibility { get; set; }
        public Visibility RandomizeSectionVisibility { get; set; }
        readonly IHRCOpenFileService _openFileService;
        readonly IMessageBoxService _messageBoxService;
        readonly IMasterDatabaseService _databaseService;
        public double Latitude { get; set; }
        public double Longitude { get; set; }
        bool _isGeoChanged;
        [UsedImplicitly]PropertyObserver<ShipTrack> _shipTrackObserver;
        void ShipTrackChanged()
        {
            if (_shipTrackObserver != null) _shipTrackObserver.UnregisterHandler(p => p.OverrideTimestamps);
            _shipTrackObserver = null;
            if (Platform.ShipTrack != null)
            {
                Platform.ShipTrack.CheckTimestamps();
                _shipTrackObserver = new PropertyObserver<ShipTrack>(Platform.ShipTrack)
                    .RegisterHandler(p => p.OverrideTimestamps,
                                     () =>
                                     {
                                         Platform.Refresh();
                                         CheckIsSpeedEnabled();
                                     }
                    );
            }
            Platform.Refresh();
            CheckIsSpeedEnabled();
        }
        void WindowTitleChanged() { WindowTitle = string.Format("Platform Properties: {0}", Platform.PlatformName); }
        void CheckIsSpeedEnabled()
        {
            switch (Platform.SelectedTrackType)
            {
                case TrackType.Stationary:
                    IsSpeedEnabled = false;
                    break;
                case TrackType.StraightLine:
                case TrackType.PerimeterBounce:
                    IsSpeedEnabled = true;
                    break;
                case TrackType.WaypointFile:
                    IsSpeedEnabled = Platform.ShipTrack != null && Platform.ShipTrack.OverrideTimestamps;
                    break;
            }
        }
        void SelectedTrackTypeChanged()
        {
            CheckIsSpeedEnabled();
            ImportWaypointFileVisibility = _openFileService != null && Platform.SelectedTrackType == TrackType.WaypointFile ? Visibility.Visible : Visibility.Collapsed;
            RandomizeSectionVisibilityChanged();
        }
        void RandomizeSectionVisibilityChanged() { RandomizeSectionVisibility = IsPSMView || (Platform.SelectedTrackType == TrackType.WaypointFile) ? Visibility.Collapsed : Visibility.Visible; }
        #region OkCommand
        public SimpleCommand<object, EventToCommandArgs> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, EventToCommandArgs>(OkHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _ok;

        void OkHandler(EventToCommandArgs args)
        {
            if (_isGeoChanged) Platform.Geo = new Geo(Latitude, Longitude);
            if (!IsPSMView) CloseDialog(true);
            else MediatorMessage.Send(MediatorMessage.PSMPlatformChanged, Platform);
        }
        #endregion

        #region ImportWaypointFileCommand
        public SimpleCommand<object, EventToCommandArgs> ImportWaypointFileCommand { get { return _importWaypointFile ?? (_importWaypointFile = new SimpleCommand<object, EventToCommandArgs>(ImportWaypointFileHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _importWaypointFile;

        void ImportWaypointFileHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter;
            _openFileService.Title = "Import waypoint file";
            _openFileService.Filter = "Waypoint files (*.wpt)|*.wpt|Text files (*.txt)|*.txt|All files (*.*)|*.*";
            var result = _openFileService.ShowDialog(null);
            if (!result.HasValue || !result.Value) return;
            try
            {
                if (Platform.ShipTrack != null) _databaseService.Context.ShipTracks.Remove(Platform.ShipTrack);
                var shipTrack = ShipTrack.ReadWaypointFile(_openFileService.FileName);
                _databaseService.Context.ShipTracks.Add(shipTrack);
                shipTrack.Platform = Platform;
                Platform.ShipTrack = shipTrack;
            }
            catch (Exception ex)
            {
                _messageBoxService.ShowError(ex.Message);
            }
        }
        #endregion
    }
}

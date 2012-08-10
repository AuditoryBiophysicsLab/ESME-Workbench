using System;
using System.ComponentModel.DataAnnotations;
using System.Globalization;
using System.Linq;
using System.Windows.Data;
using ESME.Database;
using ESME.Locations;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Scenarios
{
    [NotifyPropertyChanged]
    public class Mode : IHaveGuid
    {
        public Mode() {}

        public Mode(Mode mode)
        {
            PSMModeGuid = mode.PSMModeGuid;
            ModeName = mode.ModeName;
            ModeType = mode.ModeType;
            ActiveTime = mode.ActiveTime;
            Depth = mode.Depth;
            SourceLevel = mode.SourceLevel;
            LowFrequency = mode.LowFrequency;
            HighFrequency = mode.HighFrequency;
            PulseInterval = mode.PulseInterval;
            PulseLength = mode.PulseLength;
            HorizontalBeamWidth = mode.HorizontalBeamWidth;
            VerticalBeamWidth = mode.VerticalBeamWidth;
            DepressionElevationAngle = mode.DepressionElevationAngle;
            RelativeBeamAngle = mode.RelativeBeamAngle;
            MaxPropagationRadius = mode.MaxPropagationRadius;
        }
        [Key, Initialize] public Guid Guid { get; set; }
        public string PSMModeGuid { get; set; }
        public string ModeName { get; set; }
        public string ModeType { get; set; }
        public float? ActiveTime { get; set; }

        /// <summary>
        /// Depth offset from the platform, in meters
        /// </summary>
        public float? Depth { get; set; }

        /// <summary>
        /// Source level in dB SPL re: 1 μPa
        /// </summary>
        public float SourceLevel { get; set; }

        /// <summary>
        /// Lowest frequency of this mode, in Hz
        /// </summary>
        public float LowFrequency { get; set; }

        /// <summary>
        /// Highest frequency of this mode, in Hz
        /// </summary>
        public float HighFrequency { get; set; }

        /// <summary>
        /// Time between the start of sequential pulses
        /// </summary>
        public DbTimeSpan PulseInterval { get; set; }

        /// <summary>
        /// The length of time a single pulse is transmitting
        /// </summary>
        public DbTimeSpan PulseLength { get; set; }

        /// <summary>
        /// Horizontal beam width of this mode, in degrees.  
        /// The beam is assumed to spread symmetrically for half this width to either side of the beam center
        /// </summary>
        public float HorizontalBeamWidth { get; set; }

        /// <summary>
        /// Vertical beam width of this mode, in degrees
        /// The beam is assumed to spread symmetrically for half this width to either side of the beam center
        /// </summary>
        public float VerticalBeamWidth { get; set; }

        /// <summary>
        /// The beam center in the vertical plane.  Positive values are toward the sea floor.
        /// </summary>
        public float DepressionElevationAngle { get; set; }

        /// <summary>
        /// The beam center in the horizontal plane, in degrees from true north, relative to the current heading of the platform on which this mode is hosted. 
        /// </summary>
        public float RelativeBeamAngle { get; set; }

        /// <summary>
        /// The maximum distance to calculate the transmission loss for this mode
        /// </summary>
        public float MaxPropagationRadius { get; set; }

        public virtual Source Source { get; set; }
        [Initialize] public virtual ObservableList<LogEntry> Logs { get; set; }
        [Initialize] public virtual ObservableList<TransmissionLoss> TransmissionLosses { get; set; }

        [NotMapped] public string PSMName { get { return string.Format("{0}:{1}:{2}", Source.Platform.PlatformName, Source.SourceName, ModeName); } }
        [NotMapped] public bool IsNew { get; set; }

        [NotMapped] public object LayerControl
        {
            get { return _layerControl; }
            set
            {
                _layerControl = value;
                MediatorMessage.Send(MediatorMessage.ModeBoundToLayer, this);
            }
        }

        object _layerControl;

        [NotMapped] public int ModeID { get; set; }

        public void CreateMapLayers() { throw new NotImplementedException(); }
        public void RemoveMapLayers() { throw new NotImplementedException(); }

        #region DeleteModeCommand
        public SimpleCommand<object, EventToCommandArgs> DeleteModeCommand { get { return _deleteMode ?? (_deleteMode = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeleteMode, this))); } }
        SimpleCommand<object, EventToCommandArgs> _deleteMode;
        #endregion

        #region RecalculateModeCommand
        public SimpleCommand<object, EventToCommandArgs> RecalculateModeCommand { get { return _recalculateMode ?? (_recalculateMode = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.RecalculateMode, this))); } }
        SimpleCommand<object, EventToCommandArgs> _recalculateMode;
        #endregion

        #region ModePropertiesCommand
        public SimpleCommand<object, EventToCommandArgs> ModePropertiesCommand { get { return _modeProperties ?? (_modeProperties = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.ModeProperties, this))); } }
        SimpleCommand<object, EventToCommandArgs> _modeProperties;
        #endregion

        public void Delete()
        {
            Source.Modes.Remove(this);
            foreach (var tl in TransmissionLosses.ToList()) tl.Delete();
            Scenario.Database.Context.Modes.Remove(this);
        }

        #region Layer Move commands

        #region MoveLayerToFrontCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerToFrontCommand { get { return _moveLayerToFront ?? (_moveLayerToFront = new SimpleCommand<object, EventToCommandArgs>(MoveLayerToFront)); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerToFront;

        void MoveLayerToFront(EventToCommandArgs args)
        {
            foreach (var tl in TransmissionLosses) tl.LayerSettings.MoveLayerToFront();
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion

        #region MoveLayerForwardCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerForwardCommand { get { return _moveLayerForward ?? (_moveLayerForward = new SimpleCommand<object, EventToCommandArgs>(MoveLayerForward)); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerForward;

        void MoveLayerForward(EventToCommandArgs args)
        {
            foreach (var tl in TransmissionLosses) tl.LayerSettings.MoveLayerForward();
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion

        #region MoveLayerBackwardCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerBackwardCommand { get { return _moveLayerBackward ?? (_moveLayerBackward = new SimpleCommand<object, EventToCommandArgs>(MoveLayerBackward)); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerBackward;

        void MoveLayerBackward(EventToCommandArgs args)
        {
            foreach (var tl in TransmissionLosses) tl.LayerSettings.MoveLayerBackward();
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion

        #region MoveLayerToBackCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerToBackCommand { get { return _moveLayerToBack ?? (_moveLayerToBack = new SimpleCommand<object, EventToCommandArgs>(MoveLayerToBack)); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerToBack;

        void MoveLayerToBack(EventToCommandArgs args)
        {
            foreach (var tl in TransmissionLosses) tl.LayerSettings.MoveLayerToBack();
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion

        #endregion

        public bool IsAcousticallyEquivalentTo(Mode other)
        {
            var myDepth = Source.Platform.Depth;
            if (Depth.HasValue) myDepth += Depth.Value;
            var otherDepth = other.Source.Platform.Depth;
            if (other.Depth.HasValue) otherDepth += other.Depth.Value;
            if (Math.Abs(myDepth - otherDepth) > 0.001) return false;
            if (Math.Abs(VerticalBeamWidth - other.VerticalBeamWidth) > 0.1) return false;
            if (Math.Abs(DepressionElevationAngle - other.DepressionElevationAngle) > 0.1) return false;
            if (Math.Abs(HighFrequency - other.HighFrequency) > 0.1) return false;
            return Math.Abs(LowFrequency - other.LowFrequency) <= 0.1;
        }

        public int GetHashCode(Mode obj) { return obj.GetHashCode(); }
    }

    [ValueConversion(typeof(Mode), typeof(string))]
    public class ModeGroupingConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            var mode = (Mode)value;
            var depth = mode.Source.Platform.Depth;
            if (mode.Depth.HasValue) depth += mode.Depth.Value;
            return string.Format("{0}Hz, {1}Hz, {2}m, {3}deg, {4}deg", mode.HighFrequency, mode.LowFrequency, depth, mode.VerticalBeamWidth, mode.DepressionElevationAngle);
        }
        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture) { throw new NotImplementedException(); }
    }
}
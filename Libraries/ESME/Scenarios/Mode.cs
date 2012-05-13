using System;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using ESME.Database;
using ESME.Locations;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Scenarios
{
    public class Mode : IHaveGuid, IEquatable<Mode>
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string PSMModeGuid { get; set; }
        public string ModeName { get; set; }
        public string ModeType { get; set; }
        public float? ActiveTime { get; set; }
        public float? Depth { get; set; }
        public float SourceLevel { get; set; }
        public float LowFrequency { get; set; }
        public float HighFrequency { get; set; }
        public DbTimeSpan PulseInterval { get; set; }
        public DbTimeSpan PulseLength { get; set; }
        public float HorizontalBeamWidth { get; set; }
        public float VerticalBeamWidth { get; set; }
        public float DepressionElevationAngle { get; set; }
        public float RelativeBeamAngle { get; set; }
        public float MaxPropagationRadius { get; set; }

        public virtual Source Source { get; set; }
        [Initialize] public virtual ObservableList<LogEntry> Logs { get; set; }
        [Initialize] public virtual ObservableList<TransmissionLoss> TransmissionLosses { get; set; }

        [NotMapped] public string PSMName { get { return string.Format("{0}:{1}:{2}", Source.Platform.PlatformName, Source.SourceName, ModeName); } }
        [NotMapped] public bool IsNew { get; set; }
        [NotMapped]
        public object LayerControl
        {
            get { return _layerControl; }
            set
            {
                _layerControl = value;
                MediatorMessage.Send(MediatorMessage.ModeBoundToLayer, this);
            }
        }
        object _layerControl;


        /// <summary>
        /// Indicates whether the current object is equal to another object of the same type.
        /// </summary>
        /// <returns>
        /// true if the current object is equal to the <paramref name="other"/> parameter; otherwise, false.
        /// </returns>
        /// <param name="other">An object to compare with this object.</param>
        public bool Equals(Mode other)
        {
            if (Guid != other.Guid) return false;
            var mydepth=0f;
            if (Depth.HasValue)
                mydepth = Depth.Value;
            if ((Source == null || Source.Platform == null) && (other.Source == null || other.Source.Platform == null)) return true;
            mydepth += Source.Platform.Depth;
            var otherdepth = 0f;
            if (other.Depth.HasValue)
                otherdepth = other.Depth.Value;
            otherdepth += other.Source.Platform.Depth;
            return (Math.Abs(mydepth - otherdepth) < .001);
        }

        public void CreateMapLayers() { throw new NotImplementedException(); }
        public void RemoveMapLayers() { throw new NotImplementedException(); }

        #region DeleteModeCommand
        public SimpleCommand<object, EventToCommandArgs> DeleteModeCommand { get { return _deleteMode ?? (_deleteMode = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeleteMode, this))); } }
        SimpleCommand<object, EventToCommandArgs> _deleteMode;
        #endregion

        #region RecalculateModeCommand
        public SimpleCommand<object, EventToCommandArgs> RecalculateModeCommand { get { return _recalculateMode ?? (_recalculateMode = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.RecalculateMode,this))); } }
        SimpleCommand<object, EventToCommandArgs> _recalculateMode;
        #endregion

        #region ModePropertiesCommand
        public SimpleCommand<object, EventToCommandArgs> ModePropertiesCommand { get { return _modeProperties ?? (_modeProperties = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.ModeProperties, this))); } }
        SimpleCommand<object, EventToCommandArgs> _modeProperties;
        #endregion
        public void Delete()
        {
            foreach (var tl in TransmissionLosses.ToList()) tl.Delete();
            Source.Modes.Remove(this);
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
    }
}
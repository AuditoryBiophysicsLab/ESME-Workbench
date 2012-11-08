using System;
using ESME.Scenarios;
using HRC.Aspects;
using HRC.Validation;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Scenarios
{
    [NotifyPropertyChanged]
    public class ModePropertiesViewModel : ValidatingViewModel
    {
        readonly Mode _mode;
        const string TimeSpanFormatString = @"hh\:mm\:ss\.fff";
        public ModePropertiesViewModel(Mode mode)
        {
            _mode = mode;
            ModeName = _mode.ModeName;
            ModeType = _mode.ModeType;
            Depth = _mode.Depth;
            SourceLevel = _mode.SourceLevel;
            LowFrequency = _mode.LowFrequency;
            HighFrequency = _mode.HighFrequency;
            PulseIntervalString = _mode.PulseInterval != null ? ((TimeSpan)_mode.PulseInterval).ToString(TimeSpanFormatString) : null;
            PulseLengthString = _mode.PulseLength != null ? ((TimeSpan)_mode.PulseLength).ToString(TimeSpanFormatString):null;
            HorizontalBeamWidth = _mode.HorizontalBeamWidth;
            VerticalBeamWidth = _mode.VerticalBeamWidth;
            DepressionElevationAngle = _mode.DepressionElevationAngle;
            RelativeBeamAngle = _mode.RelativeBeamAngle;
            MaxPropagationRadius = _mode.MaxPropagationRadius;
            WindowTitle = string.Format("Mode properties: {0}", _mode.ModeName);
            AddValidationRules(
                new ValidationRule<ModePropertiesViewModel>
                {
                    PropertyName = "Depth",
                    Description = "Cannot be negative",
                    IsRuleValid = (target, rule) => target.Depth.HasValue && target.Depth.Value >= 0.0f,
                }
                ,
                new ValidationRule<ModePropertiesViewModel>
                {
                    PropertyName = "SourceLevel",
                    Description = "Must be greater than zero",
                    IsRuleValid = (target, rule) => target.SourceLevel > 0,
                },
                new ValidationRule<ModePropertiesViewModel>
                {
                    PropertyName = "HighFrequency",
                    Description = "Must be greater than zero",
                    IsRuleValid = (target, rule) => target.HighFrequency > 0,
                },
                new ValidationRule<ModePropertiesViewModel>
                {
                    PropertyName = "PulseIntervalString",
                    Description = "Must be a valid, non-negative time span value in the format hh:mm:ss.fff where 00 <= hh <= 23; 00 <= mm <= 59; 00 <= ss <= 59; 000 <= fff <= 999",
                    IsRuleValid = (target, rule) =>
                    {
                        if (string.IsNullOrEmpty(target.PulseIntervalString)) return false;
                        TimeSpan timeSpan;
                        var isOK = TimeSpan.TryParseExact(target.PulseIntervalString, TimeSpanFormatString, null, out timeSpan);
                        return isOK && timeSpan.Ticks > 0;
                    },
                },
                new ValidationRule<ModePropertiesViewModel>
                {
                    PropertyName = "PulseLengthString",
                    Description = "Must be a valid, non-negative time span value in the format hh:mm:ss.fff where 00 <= hh <= 23; 00 <= mm <= 59; 00 <= ss <= 59; 000 <= fff <= 999",
                    IsRuleValid = (target, rule) =>
                    {
                        if (string.IsNullOrEmpty(target.PulseLengthString)) return false;
                        TimeSpan timeSpan;
                        var isOK = TimeSpan.TryParseExact(target.PulseLengthString, TimeSpanFormatString, null, out timeSpan);
                        return isOK && timeSpan.Ticks > 0;
                    },
                },
                new ValidationRule<ModePropertiesViewModel>
                {
                    PropertyName = "RelativeBeamAngle",
                    Description = "Must be between -180 and 180, inclusive",
                    IsRuleValid = (target, rule) => -180 <= target.RelativeBeamAngle && target.RelativeBeamAngle <= 180,
                },
                new ValidationRule<ModePropertiesViewModel>
                {
                    PropertyName = "HorizontalBeamWidth",
                    Description = "Must be a positive value less than or equal to 360",
                    IsRuleValid = (target, rule) => 0 < target.HorizontalBeamWidth && target.HorizontalBeamWidth <= 360,
                },
                new ValidationRule<ModePropertiesViewModel>
                {
                    PropertyName = "VerticalBeamWidth",
                    Description = "Must be a positive value less than or equal to 180",
                    IsRuleValid = (target, rule) => 0 < target.VerticalBeamWidth && target.VerticalBeamWidth <= 180,
                },
                new ValidationRule<ModePropertiesViewModel>
                {
                    PropertyName = "DepressionElevationAngle",
                    Description = "Must be between -90 and 90, inclusive",
                    IsRuleValid = (target, rule) => -90 <= target.DepressionElevationAngle && target.DepressionElevationAngle <= 90,
                },
                new ValidationRule<ModePropertiesViewModel>
                {
                    PropertyName = "MaxPropagationRadius",
                    Description = "Must be a positive value",
                    IsRuleValid = (target, rule) => target.MaxPropagationRadius > 0,
                });
        }

        public string WindowTitle { get; set; }
        public string ModeName { get; set; }
        public string ModeType { get; set; }
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
        public string PulseIntervalString { get; set; }

        public string PulseLengthString { get; set; }

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

        public bool AcousticPropertiesHaveChanged { get; private set; }
        bool HaveAcousticPropertiesChanged()
        {
            if (_mode.Depth.HasValue != Depth.HasValue) return true;
            if (_mode.Depth.HasValue && Depth.HasValue && (_mode.Depth.Value != Depth.Value)) return true;
            if (Math.Abs(VerticalBeamWidth - _mode.VerticalBeamWidth) > 0.1) return true;
            if (Math.Abs(DepressionElevationAngle - _mode.DepressionElevationAngle) > 0.1) return true;
            if (Math.Abs(HighFrequency - _mode.HighFrequency) > 0.1) return true;
            return Math.Abs(LowFrequency - _mode.LowFrequency) > 0.1;
        }
        public bool RadiusHasChanged { get; private set; }

        public bool IsPSMView { get; set; }




        #region OkCommand
        public SimpleCommand<object, EventToCommandArgs> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, EventToCommandArgs>(o => IsOkCommandEnabled, OkHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _ok;

        bool IsOkCommandEnabled
        {
            get { return !IsPSMView || IsValid; }
        }

        void OkHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter; 
            if (IsPSMView) { MediatorMessage.Send(MediatorMessage.PSMModeChanged, _mode); }
            else
            {
                //var parameter = args.CommandParameter;
                AcousticPropertiesHaveChanged = HaveAcousticPropertiesChanged();
                RadiusHasChanged = Math.Abs(MaxPropagationRadius - _mode.MaxPropagationRadius) > 0.1;
                _mode.ModeName = ModeName;
                _mode.ModeType = ModeType;
                _mode.Depth = Depth;
                _mode.SourceLevel = SourceLevel;
                _mode.LowFrequency = LowFrequency;
                _mode.HighFrequency = HighFrequency;
                _mode.PulseInterval = TimeSpan.ParseExact(PulseIntervalString, TimeSpanFormatString, null);
                _mode.PulseLength = TimeSpan.ParseExact(PulseLengthString, TimeSpanFormatString, null);
                _mode.HorizontalBeamWidth = HorizontalBeamWidth;
                _mode.VerticalBeamWidth = VerticalBeamWidth;
                _mode.DepressionElevationAngle = DepressionElevationAngle;
                _mode.RelativeBeamAngle = RelativeBeamAngle;
                _mode.MaxPropagationRadius = MaxPropagationRadius;
                CloseActivePopUpCommand.Execute(true);
            }
        }
        #endregion
        #region CancelCommand
        public SimpleCommand<object, EventToCommandArgs> CancelCommand { get { return _cancel ?? (_cancel = new SimpleCommand<object, EventToCommandArgs>(CancelHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _cancel;

        void CancelHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter;
            if (IsPSMView) { }
            else
            {
                CloseActivePopUpCommand.Execute(false);    
            }
        }
        #endregion
        
    }
}

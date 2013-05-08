using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using ESME.Plugins;
using ESME.Scenarios;
using HRC;
using HRC.Aspects;
using HRC.Validation;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Scenarios
{
    public class ModePropertiesViewModel : ValidatingViewModel
    {
        readonly Mode _editedMode;
        readonly Mode _originalMode;
        const string TimeSpanFormatString = @"hh\:mm\:ss\.fff";
        public static IPluginManagerService PluginManagerService { get; set; }
        [UsedImplicitly] PropertyObserver<ModePropertiesViewModel> _propertyObserver;

        public ModePropertiesViewModel(Mode mode)
        {
            _editedMode = mode;
            _originalMode = new Mode(mode);
            ModeName = _editedMode.ModeName;
            ModeType = _editedMode.ModeType;
            Depth = _editedMode.Depth;
            SourceLevel = _editedMode.SourceLevel;
            LowFrequency = _editedMode.LowFrequency;
            HighFrequency = _editedMode.HighFrequency;
            PulseIntervalString = _editedMode.PulseInterval != null ? ((TimeSpan)_editedMode.PulseInterval).ToString(TimeSpanFormatString) : null;
            PulseLengthString = _editedMode.PulseLength != null ? ((TimeSpan)_editedMode.PulseLength).ToString(TimeSpanFormatString) : null;
            HorizontalBeamWidth = _editedMode.HorizontalBeamWidth;
            VerticalBeamWidth = _editedMode.VerticalBeamWidth;
            DepressionElevationAngle = _editedMode.DepressionElevationAngle;
            RelativeBeamAngle = _editedMode.RelativeBeamAngle;
            MaxPropagationRadius = _editedMode.MaxPropagationRadius;
            ValidRadialCounts = new List<string> { "Auto", "4", "8", "16", "32", "64", "128" };
            RadialCountString = _editedMode.RadialCount == 0 ? ValidRadialCounts[0] : _editedMode.RadialCount.ToString(CultureInfo.InvariantCulture);
            SideLobeAttenuation = _editedMode.SideLobeAttenuation;

            AvailableTransmissionLossEngines.AddRange(from key in PluginManagerService[PluginType.TransmissionLossCalculator].Keys
                                                      select (TransmissionLossCalculatorPluginBase)PluginManagerService[PluginType.TransmissionLossCalculator][key].DefaultPlugin);

            SelectedTransmissionLossEngine = _editedMode.GetTransmissionLossPlugin(PluginManagerService);

            _propertyObserver = new PropertyObserver<ModePropertiesViewModel>(this)
                .RegisterHandler(p => p.SelectedTransmissionLossEngine, () => { });
            WindowTitle = string.Format("Mode properties: {0}", _editedMode.ModeName);
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

        public string RadialCountString { get; set; }
        public List<string> ValidRadialCounts { get; private set; }

        /// <summary>
        /// The side lobe attenuation, in dB
        /// </summary>
        public float SideLobeAttenuation { get; set; }
        public bool IsPSMView { get; set; }

        [Initialize] public List<TransmissionLossCalculatorPluginBase> AvailableTransmissionLossEngines { get; set; }
        public TransmissionLossCalculatorPluginBase SelectedTransmissionLossEngine { get; set; }

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
            _editedMode.ModeName = ModeName;
            _editedMode.ModeType = ModeType;
            _editedMode.Depth = Depth;
            _editedMode.SourceLevel = SourceLevel;
            _editedMode.LowFrequency = LowFrequency;
            _editedMode.HighFrequency = HighFrequency;
            _editedMode.PulseInterval = TimeSpan.ParseExact(PulseIntervalString, TimeSpanFormatString, null);
            _editedMode.PulseLength = TimeSpan.ParseExact(PulseLengthString, TimeSpanFormatString, null);
            _editedMode.HorizontalBeamWidth = HorizontalBeamWidth;
            _editedMode.VerticalBeamWidth = VerticalBeamWidth;
            _editedMode.DepressionElevationAngle = DepressionElevationAngle;
            _editedMode.RelativeBeamAngle = RelativeBeamAngle;
            _editedMode.MaxPropagationRadius = MaxPropagationRadius;
            _editedMode.TransmissionLossPluginType = SelectedTransmissionLossEngine.PluginIdentifier.Type;
            _editedMode.SideLobeAttenuation = SideLobeAttenuation;
            _editedMode.RadialCount = RadialCountString == "Auto" ? 0 : int.Parse(RadialCountString);
            if (!_originalMode.IsAcousticallyEquivalentTo(_editedMode) || _originalMode.TransmissionLossPluginType != _editedMode.TransmissionLossPluginType) _editedMode.TransmissionLosses.ForEach(tl => tl.Modes.Remove(_editedMode));
            if (IsPSMView) MediatorMessage.Send(MediatorMessage.PSMModeChanged, _editedMode);
            else CloseActivePopUpCommand.Execute(true);
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

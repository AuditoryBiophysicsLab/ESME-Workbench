using System;
using System.Collections.Generic;
using System.ComponentModel;
using Cinch;
using ESME.Model;
using ESME.NEMO;
using ESME.TransmissionLoss;
using HRC.Navigation;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class TransmissionLossJobViewModel : EditableValidatingViewModelBase, IHasIDField
    {
        #region public LabeledDataWrapper<double> Latitude { get; private set; }

        static readonly PropertyChangedEventArgs LatitudeChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.Latitude);
        LabeledDataWrapper<double> _latitude;

        public LabeledDataWrapper<double> Latitude
        {
            get { return _latitude; }
            private set
            {
                if (_latitude == value) return;
                _latitude = value;
                _latitude.ValidationRules.Add(new SimpleRule("DataValue", "Latitude must be in the range -90 to +90", domObj =>
                                                                                                                      {
                                                                                                                          var obj = (DataWrapper<double>) domObj;
                                                                                                                          return ((obj.DataValue < -90) || (90 < obj.DataValue));
                                                                                                                      }));
                NotifyPropertyChanged(LatitudeChangedEventArgs);
            }
        }

        #endregion

        #region public LabeledDataWrapper<double> Longitude { get; private set; }

        static readonly PropertyChangedEventArgs LongitudeChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.Longitude);
        LabeledDataWrapper<double> _longitude;

        public LabeledDataWrapper<double> Longitude
        {
            get { return _longitude; }
            private set
            {
                if (_longitude == value) return;
                _longitude = value;
                _longitude.ValidationRules.Add(new SimpleRule("DataValue", "Longitude must be in the range -180 to +180", domObj =>
                                                                                                                          {
                                                                                                                              var obj = (DataWrapper<double>) domObj;
                                                                                                                              return ((obj.DataValue < -180) || (180 < obj.DataValue));
                                                                                                                          }));
                NotifyPropertyChanged(LongitudeChangedEventArgs);
            }
        }

        #endregion

        #region public LabeledDataWrapper<float> SourceDepth { get; private set; }

        static readonly PropertyChangedEventArgs SourceDepthChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.SourceDepth);
        LabeledDataWrapper<float> _sourceDepth;

        public LabeledDataWrapper<float> SourceDepth
        {
            get { return _sourceDepth; }
            private set
            {
                if (_sourceDepth == value) return;
                _sourceDepth = value;
                _sourceDepth.ValidationRules.Add(new SimpleRule("DataValue", "SourceDepth must be greater than or equal to zero", domObj =>
                                                                                                                                  {
                                                                                                                                      var obj = (DataWrapper<float>) domObj;
                                                                                                                                      return (obj.DataValue < 0);
                                                                                                                                  }));
                NotifyPropertyChanged(SourceDepthChangedEventArgs);
            }
        }

        #endregion

        #region public LabeledDataWrapper<float> LowFrequency { get; private set; }

        static readonly PropertyChangedEventArgs LowFrequencyChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.LowFrequency);
        LabeledDataWrapper<float> _lowFrequency;

        public LabeledDataWrapper<float> LowFrequency
        {
            get { return _lowFrequency; }
            private set
            {
                if (_lowFrequency == value) return;
                _lowFrequency = value;
                _lowFrequency.ValidationRules.Add(new SimpleRule("DataValue", "LowFrequency must be greater than zero", domObj =>
                                                                                                                        {
                                                                                                                            var obj = (DataWrapper<float>) domObj;
                                                                                                                            return (obj.DataValue <= 0);
                                                                                                                        }));
                _lowFrequency.ValidationRules.Add(new SimpleRule("DataValue", "LowFrequency must be less than HighFrequency", domObj =>
                                                                                                                              {
                                                                                                                                  var obj = (DataWrapper<float>) domObj;
                                                                                                                                  var parent = (TransmissionLossJobViewModel) obj.ParentViewModel;
                                                                                                                                  return (obj.DataValue >= parent.HighFrequency.DataValue);
                                                                                                                              }));
                NotifyPropertyChanged(LowFrequencyChangedEventArgs);
            }
        }

        #endregion

        #region public LabeledDataWrapper<float> HighFrequency { get; private set; }

        static readonly PropertyChangedEventArgs HighFrequencyChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.HighFrequency);
        LabeledDataWrapper<float> _highFrequency;

        public LabeledDataWrapper<float> HighFrequency
        {
            get { return _highFrequency; }
            private set
            {
                if (_highFrequency == value) return;
                _highFrequency = value;
                _highFrequency.ValidationRules.Add(new SimpleRule("DataValue", "HighFrequency must be greater than zero", domObj =>
                                                                                                                          {
                                                                                                                              var obj = (DataWrapper<float>) domObj;
                                                                                                                              return (obj.DataValue <= 0);
                                                                                                                          }));
                _highFrequency.ValidationRules.Add(new SimpleRule("DataValue", "HighFrequency must be greater than LowFrequency", domObj =>
                                                                                                                                  {
                                                                                                                                      var obj = (DataWrapper<float>) domObj;
                                                                                                                                      var parent = (TransmissionLossJobViewModel) obj.ParentViewModel;
                                                                                                                                      return (obj.DataValue <= parent.LowFrequency.DataValue);
                                                                                                                                  }));
                NotifyPropertyChanged(HighFrequencyChangedEventArgs);
            }
        }

        #endregion

        #region public LabeledDataWrapper<float> VerticalBeamWidth { get; private set; }

        static readonly PropertyChangedEventArgs VerticalBeamWidthChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.VerticalBeamWidth);
        LabeledDataWrapper<float> _verticalBeamWidth;

        public LabeledDataWrapper<float> VerticalBeamWidth
        {
            get { return _verticalBeamWidth; }
            private set
            {
                if (_verticalBeamWidth == value) return;
                _verticalBeamWidth = value;
                _verticalBeamWidth.ValidationRules.Add(new SimpleRule("DataValue", "VerticalBeamWidth must be between zero and +180", domObj =>
                                                                                                                                      {
                                                                                                                                          var obj = (DataWrapper<float>) domObj;
                                                                                                                                          return ((0 <= obj.DataValue) && (180 < obj.DataValue));
                                                                                                                                      }));
                NotifyPropertyChanged(VerticalBeamWidthChangedEventArgs);
            }
        }

        #endregion

        #region public LabeledDataWrapper<float> DepressionElevationAngle { get; private set; }

        static readonly PropertyChangedEventArgs DepressionElevationAngleChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.DepressionElevationAngle);
        LabeledDataWrapper<float> _depressionElevationAngle;

        public LabeledDataWrapper<float> DepressionElevationAngle
        {
            get { return _depressionElevationAngle; }
            private set
            {
                if (_depressionElevationAngle == value) return;
                _depressionElevationAngle = value;
                _depressionElevationAngle.ValidationRules.Add(new SimpleRule("DataValue", "DepressionElevationAngle must be in the range -90 to +90", domObj =>
                                                                                                                                                      {
                                                                                                                                                          var obj = (DataWrapper<float>) domObj;
                                                                                                                                                          return ((obj.DataValue < -90) || (90 < obj.DataValue));
                                                                                                                                                      }));
                NotifyPropertyChanged(DepressionElevationAngleChangedEventArgs);
            }
        }

        #endregion

        #region public LabeledDataWrapper<float> Radius { get; private set; }

        static readonly PropertyChangedEventArgs RadiusChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.Radius);
        LabeledDataWrapper<float> _radius;

        public LabeledDataWrapper<float> Radius
        {
            get { return _radius; }
            private set
            {
                if (_radius == value) return;
                _radius = value;
                _radius.ValidationRules.Add(new SimpleRule("DataValue", "Radius must be greater than zero", domObj =>
                                                                                                            {
                                                                                                                var obj = (DataWrapper<float>) domObj;
                                                                                                                return (obj.DataValue <= 0);
                                                                                                            }));
                NotifyPropertyChanged(RadiusChangedEventArgs);
            }
        }

        #endregion

        #region public IEnumerable<DataWrapperBase> EditableFields { get; set; }

        static readonly PropertyChangedEventArgs EditableFieldsChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.EditableFields);
        IEnumerable<DataWrapperBase> _editableFields;

        public IEnumerable<DataWrapperBase> EditableFields
        {
            get { return _editableFields; }
            set
            {
                if (_editableFields == value) return;
                _editableFields = value;
                NotifyPropertyChanged(EditableFieldsChangedEventArgs);
            }
        }

        #endregion

        #region public bool IsEditable { get; set; }

        static readonly PropertyChangedEventArgs IsEditableChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.IsEditable);
        bool _isEditable;

        public bool IsEditable
        {
            get { return _isEditable; }
            set
            {
                if (_isEditable == value) return;
                _isEditable = value;
                foreach (DataWrapperBase wrapper in _editableFields) wrapper.IsEditable = _isEditable;
                NotifyPropertyChanged(IsEditableChangedEventArgs);
            }
        }

        #endregion

        #region public override bool IsValid { get; set; }

        public override bool IsValid
        {
            get { return base.IsValid && DataWrapperHelper.AllValid(_editableFields); }
        }

        #endregion

        #region public string Name { get; set; }

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.Name);
        string _name;

        public string Name
        {
            get { return _name; }
            set
            {
                if (_name == value) return;
                _name = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }

        #endregion

        #region public string Metadata { get; set; }

        static readonly PropertyChangedEventArgs MetadataChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.Metadata);
        string _metadata;

        public string Metadata
        {
            get { return _metadata; }
            set
            {
                if (_metadata == value) return;
                _metadata = value;
                NotifyPropertyChanged(MetadataChangedEventArgs);
            }
        }

        #endregion

        #region public float SourceLevel { get; set; }

        static readonly PropertyChangedEventArgs SourceLevelChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.SourceLevel);
        float _sourceLevel;

        /// <summary>
        /// Source Level in dB SPL re: 1uPa
        /// </summary>
        public float SourceLevel
        {
            get { return _sourceLevel; }
            set
            {
                if (_sourceLevel == value) return;
                _sourceLevel = value;
                NotifyPropertyChanged(SourceLevelChangedEventArgs);
            }
        }

        #endregion

        readonly int _maxCalculationDepth;
        readonly EarthCoordinate _location;
        readonly NemoMode _nemoMode;
        readonly int _radialCount;
        public TransmissionLossJobViewModel(EarthCoordinate location, NemoMode nemoMode, int radialCount, int maxCalculationDepth)
        {
            #region Create DataWrappers

            Latitude = new LabeledDataWrapper<double>(this, LatitudeChangedEventArgs)
                       {
                           Label = "Latitude (deg)"
                       };
            Longitude = new LabeledDataWrapper<double>(this, LongitudeChangedEventArgs)
                        {
                            Label = "Longitude (deg)"
                        };
            SourceDepth = new LabeledDataWrapper<float>(this, SourceDepthChangedEventArgs)
                          {
                              Label = "Source depth (m)"
                          };
            LowFrequency = new LabeledDataWrapper<float>(this, LowFrequencyChangedEventArgs)
                           {
                               Label = "Low Frequency (Hz)"
                           };
            HighFrequency = new LabeledDataWrapper<float>(this, HighFrequencyChangedEventArgs)
                            {
                                Label = "High Frequency (Hz)"
                            };
            VerticalBeamWidth = new LabeledDataWrapper<float>(this, VerticalBeamWidthChangedEventArgs)
                                {
                                    Label = "Vertical Beam Width (deg)"
                                };
            DepressionElevationAngle = new LabeledDataWrapper<float>(this, DepressionElevationAngleChangedEventArgs)
                                       {
                                           Label = "Depression/Elevation Angle (deg)"
                                       };
            Radius = new LabeledDataWrapper<float>(this, RadiusChangedEventArgs)
                     {
                         Label = "Field radius (m)",
                     };

            #endregion

            _location = location;
            _nemoMode = nemoMode;
            _radialCount = radialCount;
            Latitude.DataValue = location.Latitude;
            Longitude.DataValue = location.Longitude;
            SourceDepth.DataValue = Math.Max(1, nemoMode.SourceDepth);
            LowFrequency.DataValue = nemoMode.LowFrequency;
            HighFrequency.DataValue = nemoMode.HighFrequency;
            VerticalBeamWidth.DataValue = nemoMode.VerticalBeamWidth;
            DepressionElevationAngle.DataValue = nemoMode.DepressionElevationAngle;
            Radius.DataValue = nemoMode.Radius;
            SourceLevel = nemoMode.SourceLevel;
            _maxCalculationDepth = maxCalculationDepth;

            _editableFields = DataWrapperHelper.GetWrapperProperties(this);
        }

        public TransmissionLossJob TransmissionLossJob
        {
            get
            {
                return new TransmissionLossJob
                       {
                           SoundSource = new SoundSource(_location, _nemoMode, _radialCount)
                                         {
                                             Name = Name,
                                             Radius = (int) Radius.DataValue,
                                             SourceLevel = SourceLevel,
                                         },
                           MaxDepth = _maxCalculationDepth,
                           Name = Name,
                           Metadata = Metadata,
                           IDField = IDField,
                       };
            }
        }

        #region OKCommand

        SimpleCommand<object, object> _okCommand;

        public SimpleCommand<object, object> OkCommand
        {
            get { return _okCommand ?? (_okCommand = new SimpleCommand<object, object>(delegate { return IsValid; }, delegate { CloseActivePopUpCommand.Execute(true); })); }
        }

        #endregion

        #region IHasIDField Members

        public ulong IDField { get; set; }

        #endregion
    }
}
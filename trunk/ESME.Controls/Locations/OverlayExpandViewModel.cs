using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Globalization;
using System.Windows.Controls;
using Cinch;
using ESME.Metadata;

namespace ESME.Views.Locations
{
    public class OverlayExpandViewModel : ValidatingViewModelBase
    {
        readonly IEnumerable<DataWrapperBase> _dataWrappers;

        public OverlayExpandViewModel(NAEMOOverlayMetadata naemoOverlayMetadata)
        {
            NAEMOOverlayMetadata = naemoOverlayMetadata;

            // Call all of the Create...Wrapper() methods here
            CreateBufferSizeWrapper();
            
            // Call this last
            _dataWrappers = DataWrapperHelper.GetWrapperProperties(this);
        }

        #region public bool IsValid { get; }

        public override bool IsValid
        {
            get
            {
                var newValue = base.IsValid && DataWrapperHelper.AllValid(_dataWrappers);
                if (newValue != _isValid)
                {
                    _isValid = newValue;
                    NotifyPropertyChanged(IsValidChangedEventArgs);
                }
                return newValue;
            }
        }

        static readonly PropertyChangedEventArgs IsValidChangedEventArgs = ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.IsValid);
        bool _isValid;
        #endregion

        #region public float Buffer { get; set; }

        public float Buffer
        {
            get { return _buffer; }
            set
            {
                if (_buffer == value) return;
                _buffer = value;
                NotifyPropertyChanged(BufferChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BufferChangedEventArgs = ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.Buffer);
        float _buffer;

        #endregion

        public override string this[string propertyName]
        {
            get
            {
                switch (propertyName)
                {
                    case "BufferSize":
                        return BufferSize.Error;
                    default:
                        return null;
                }
            }
        }

        #region public DataWrapper<float> BufferSize { get; set; }

        public DataWrapper<float> BufferSize
        {
            get { return _bufferSize; }
            private set
            {
                if (_bufferSize == value) return;
                _bufferSize = value;
                NotifyPropertyChanged(BufferSizeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BufferSizeChangedEventArgs = ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.BufferSize);
        DataWrapper<float> _bufferSize;

        // Make sure you call CreateBufferSizeWrapper() in the class constructor
        public void CreateBufferSizeWrapper()
        {
            BufferSize = new DataWrapper<float>(this, BufferSizeChangedEventArgs) {IsEditable = true};
            _bufferSize.AddRule(new SimpleRule("Buffer Size", "The size must be greater than zero", source =>
            {
                var dataValue = ((DataWrapper<float>)source).DataValue;
                // This must return true if the value is NOT valid
                return dataValue < 0;
            }));
            // Add any additional validation rules here
        }
        #endregion

        #region public NAEMOOverlayMetadata NAEMOOverlayMetadata { get; set; }

        private static readonly PropertyChangedEventArgs NAEMOOverlayMetadataChangedEventArgs =
            ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.NAEMOOverlayMetadata);

        private NAEMOOverlayMetadata _nAEMOOverlayMetadata;

        public NAEMOOverlayMetadata NAEMOOverlayMetadata
        {
            get { return _nAEMOOverlayMetadata; }
            set
            {
                if (_nAEMOOverlayMetadata == value) return;
                _nAEMOOverlayMetadata = value;
                NotifyPropertyChanged(NAEMOOverlayMetadataChangedEventArgs);
            }
        }

        #endregion

        #region public string SelectedItem { get; set; }

        private static readonly PropertyChangedEventArgs SelectedItemChangedEventArgs =
            ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.SelectedItem);

        private string _selectedItem;

        public string SelectedItem
        {
            get { return _selectedItem; }
            set
            {
                if (_selectedItem == value) return;
                _selectedItem = value;
                NotifyPropertyChanged(SelectedItemChangedEventArgs);
            }
        }

        #endregion

        #region OKCommand

        private SimpleCommand<object, object> _oK;

        public SimpleCommand<object, object> OKCommand
        {
            get
            {
                return _oK ??
                       (_oK =
                        new SimpleCommand<object, object>(delegate { return IsValid; },
                                                          delegate { OKHandler(); }));
            }
        }

        private void OKHandler()
        {
            CloseActivePopUpCommand.Execute(true);
        }

        #endregion
    }

    public class ValidRangeRule : ValidationRule
    {
        public ValidRangeRule()
        {
            Minimum = double.MinValue;
            Maximum = double.MaxValue;
        }

        public double Minimum { get; set; }

        public double Maximum { get; set; }

        public override ValidationResult Validate(object value, CultureInfo cultureInfo)
        {
            double test = 0;

            try
            {
                if (((string)value).Length > 0) test = Int32.Parse((String)value);
            }
            catch (Exception e)
            {
                return new ValidationResult(false, "Illegal characters or " + e.Message);
            }

            if ((test <= Minimum) || (test >= Maximum))
            {
                if ((Minimum == double.MinValue) && (Maximum == double.MaxValue)) return new ValidationResult(false, "Value must be a real number.");
                if (Minimum == double.MinValue) return new ValidationResult(false, "Value must be less than " + Maximum + ".");
                if (Maximum == double.MaxValue) return new ValidationResult(false, "Value must be greater than " + Minimum + ".");
                return new ValidationResult(false, "Value must be between " + Minimum + " and " + Maximum + ".");
            }
            return new ValidationResult(true, null);
        }
    }
}
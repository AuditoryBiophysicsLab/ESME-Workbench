using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using Cinch;
using ESME.TransmissionLoss;

namespace ESME.Views.TransmissionLossViewer
{
    public class TransmissionLossFieldListItemViewModel : ViewModelBase
    {
        public TransmissionLossFieldListItemViewModel(TransmissionLossField transmissionLossField)
        {
            TransmissionLossField = transmissionLossField;
            FieldName = transmissionLossField.Name;
            AcousticProperties = new ObservableCollection<LabelValuePair>
                                 {
                                     new LabelValuePair
                                     {
                                         Label = "Source Level",
                                         Value = transmissionLossField.SourceLevel.ToString("0.# dB"),
                                     },
                                     new LabelValuePair
                                     {
                                         Label = "Radius",
                                         Value = transmissionLossField.Radius.ToString("0.# m"),
                                     },
                                     new LabelValuePair
                                     {
                                         Label = "Source Depth",
                                         Value = transmissionLossField.SourceDepth.ToString("0.# m"),
                                     },
                                     new LabelValuePair
                                     {
                                         Label = "Low Frequency",
                                         Value = transmissionLossField.LowFrequency.ToString("0.## Hz"),
                                     },
                                     new LabelValuePair
                                     {
                                         Label = "High Frequency",
                                         Value = transmissionLossField.HighFrequency.ToString("0.## Hz"),
                                     },
                                     new LabelValuePair
                                     {
                                         Label = "Vertical Beam Width",
                                         Value = transmissionLossField.VerticalBeamWidth.ToString("0.# deg"),
                                     },
                                     new LabelValuePair
                                     {
                                         Label = "D/E Angle",
                                         Value = transmissionLossField.DepressionElevationAngle.ToString("0.# deg"),
                                     },
                                 };
        }

        public TransmissionLossField TransmissionLossField { get; set; }

        #region public string FieldName { get; set; }


        static readonly PropertyChangedEventArgs FieldNameChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldListItemViewModel>(x => x.FieldName);
        string _fieldName;

        public string FieldName
        {
            get { return _fieldName; }
            set
            {
                if (_fieldName == value) return;
                _fieldName = value;
                NotifyPropertyChanged(FieldNameChangedEventArgs);
            }
        }

        #endregion

        #region public ObservableCollection<LabelValuePair> AcousticProperties { get; set; }

        static readonly PropertyChangedEventArgs AcousticPropertiesChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldListItemViewModel>(x => x.AcousticProperties);
        ObservableCollection<LabelValuePair> _acousticProperties;

        public ObservableCollection<LabelValuePair> AcousticProperties
        {
            get { return _acousticProperties; }
            set
            {
                if (_acousticProperties == value) return;
                if (_acousticProperties != null) _acousticProperties.CollectionChanged -= AcousticPropertiesCollectionChanged;
                _acousticProperties = value;
                if (_acousticProperties != null) _acousticProperties.CollectionChanged += AcousticPropertiesCollectionChanged;
                NotifyPropertyChanged(AcousticPropertiesChangedEventArgs);
            }
        }

        void AcousticPropertiesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(AcousticPropertiesChangedEventArgs); }

        #endregion
    }
}
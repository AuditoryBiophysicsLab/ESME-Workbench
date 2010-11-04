using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using Cinch;
using ESME.TransmissionLoss;
using HRC.Services;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    [ExportViewModel("TransmissionLossFieldViewModel")]
    public class TransmissionLossFieldViewModel : ViewModelBase
    {
        static readonly PropertyChangedEventArgs SelectedRadialChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.SelectedRadial);
        
        readonly IViewAwareStatus _viewAwareStatus;

        bool _iAmInitialized;
        TransmissionLossField _tempField;

        [ImportingConstructor]
        public TransmissionLossFieldViewModel(IHRCSaveFileService saveFileService, IViewAwareStatus viewAwareStatus)
        {
            RegisterMediator();
            _viewAwareStatus = viewAwareStatus;
            ColorMapViewModel = ColorMapViewModel.Default;
            _viewAwareStatus.ViewLoaded += () => MediatorMessage.Send(MediatorMessage.TransmissionLossFieldViewInitialized, true);
        }

        #region public TransmissionLossField TransmissionLossField { get; set; }

        public TransmissionLossField TransmissionLossField
        {
            get { return _transmissionLossField; }
            set
            {
                if (_transmissionLossField == value) return;
                _transmissionLossField = value;
                _transmissionLossField.LoadData();
                SelectedRadial = 1;
                NotifyPropertyChanged(TransmissionLossFieldChangedEventArgs);
                NotifyPropertyChanged(RadialCountChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLossFieldChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.TransmissionLossField);
        TransmissionLossField _transmissionLossField;

        #endregion

        #region public int RadialCount { get; set; }

        public int RadialCount
        {
            get { return TransmissionLossField != null ? TransmissionLossField.Radials.Length : 0; }
        }

        static readonly PropertyChangedEventArgs RadialCountChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.RadialCount);

        #endregion

        public int SelectedRadial
        {
            get { return _selectedRadial; }
            set
            {
                if (value == _selectedRadial) return;
                _selectedRadial = value;
                NotifyPropertyChanged(SelectedRadialChangedEventArgs);
                MediatorMessage.Send(MediatorMessage.TransmissionLossRadialChanged, TransmissionLossField.Radials[_selectedRadial - 1]);
                MediatorMessage.Send(MediatorMessage.SetSelectedRadialBearing, TransmissionLossField.Radials[_selectedRadial - 1].BearingFromSource);
                //TransmissionLossRadialViewModel.TransmissionLossRadial = TransmissionLossField.Radials[_selectedRadial - 1];
            }
        }
        int _selectedRadial;


        #region public double SelectedRadialBearing { get; set; }

        public double SelectedRadialBearing
        {
            get { return _selectedRadialBearing; }
            set
            {
                if (_selectedRadialBearing == value) return;
                _selectedRadialBearing = value;
                NotifyPropertyChanged(SelectedRadialBearingChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedRadialBearingChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.SelectedRadialBearing);
        double _selectedRadialBearing;

        #endregion


        #region public ColorMapViewModel ColorMapViewModel { get; set; }

        static readonly PropertyChangedEventArgs ColorMapViewModelChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.ColorMapViewModel);
        ColorMapViewModel _colorMapViewModel;

        public ColorMapViewModel ColorMapViewModel
        {
            get { return _colorMapViewModel; }
            set
            {
                if (_colorMapViewModel == value) return;
                _colorMapViewModel = value;
                // TransmissionLossRadialViewModel.ColorMapViewModel = ColorMapViewModel;
                NotifyPropertyChanged(ColorMapViewModelChangedEventArgs);
            }
        }

        #endregion

        void RegisterMediator()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nTransmissionLossFieldViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
        }
        [MediatorMessageSink(MediatorMessage.SetSelectedRadialBearing)]
        void SetSelectedRadialBearing(double selectedRadialBearing) { SelectedRadialBearing = selectedRadialBearing; }

        [MediatorMessageSink(MediatorMessage.SaveRadialAsCSV)]
        void SaveRadialAsCSV(string fileName) { TransmissionLossField.Radials[SelectedRadial].SaveAsCSV(fileName, TransmissionLossField); }

        [MediatorMessageSink(MediatorMessage.TransmissionLossFieldChanged)]
        void TransmissionLossFieldChanged(TransmissionLossField transmissionLossField)
        {
            if (_iAmInitialized) TransmissionLossField = transmissionLossField;
            else _tempField = transmissionLossField;
        }

        [MediatorMessageSink(MediatorMessage.TransmissionLossFieldViewInitialized)]
        void TransmissionLossFieldViewInitialized(bool dummy)
        {
            _iAmInitialized = true;
            if (_tempField != null) TransmissionLossField = _tempField;
        }
    }
}
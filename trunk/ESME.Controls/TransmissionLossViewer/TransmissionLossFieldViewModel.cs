using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Windows;
using System.Windows.Forms;
using Cinch;
using ESME.TransmissionLoss;
using HRC.Navigation;
using HRC.Services;
using MEFedMVVM.ViewModelLocator;
using ESME.Views.Controls;


namespace ESME.Views.TransmissionLossViewer
{
    [ExportViewModel("TransmissionLossFieldViewModel")]
    public class TransmissionLossFieldViewModel : ViewModelBase
    {
        readonly IViewAwareStatus _viewAwareStatus;
        bool _iAmInitialized;
        TransmissionLossField _tempField;

        private RadialLookupInfo[,] LookupInfo { get; set; }

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
                //todo this is where slices get initialized.
                NotifyPropertyChanged(DepthViewChangedEventArgs);
                NotifyPropertyChanged(MeanViewChangedEventArgs);
                NotifyPropertyChanged(MaxViewChangedEventArgs);
                NotifyPropertyChanged(MinViewChangedEventArgs);

                var nativesize = _transmissionLossField.Radials[0].Ranges.Length * 2;
                LookupInfo = RadialLookupInfo.Create(_transmissionLossField,
                                                                  Math.Min(Math.Min(SystemInformation.PrimaryMonitorSize.Height,
                                                                           SystemInformation.PrimaryMonitorSize.Width), nativesize));

                DepthSlice = new TransmissionLossFieldSlice(_transmissionLossField, LookupInfo,
                                                            TransmissionLossFieldSlice.SliceType.Depth,0);//or SelectedDepth
                MinSlice = new TransmissionLossFieldSlice(_transmissionLossField,LookupInfo,TransmissionLossFieldSlice.SliceType.Minimum);
                MaxSlice = new TransmissionLossFieldSlice(_transmissionLossField, LookupInfo, TransmissionLossFieldSlice.SliceType.Maximum);
                MeanSlice = new TransmissionLossFieldSlice(_transmissionLossField, LookupInfo, TransmissionLossFieldSlice.SliceType.Mean);
                DepthView.SliceData = DepthSlice.SliceData;

            }
        }

        

        static readonly PropertyChangedEventArgs TransmissionLossFieldChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.TransmissionLossField);
        TransmissionLossField _transmissionLossField;

        #endregion
       
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

        #region public int RadialCount { get; set; }

        public int RadialCount
        {
            get { return TransmissionLossField != null ? TransmissionLossField.Radials.Length : 0; }
        }

        static readonly PropertyChangedEventArgs RadialCountChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.RadialCount);

        #endregion

        #region public int SelectedRadial {get; set; }
        
        public int SelectedRadial
        {
            get { return _selectedRadial; }
            set
            {
                _selectedRadial = value;
                NotifyPropertyChanged(SelectedRadialChangedEventArgs);

                MediatorMessage.Send(MediatorMessage.TransmissionLossRadialEarthCoordinate, new EarthCoordinate(_transmissionLossField.Latitude,_transmissionLossField.Longitude));
                MediatorMessage.Send(MediatorMessage.TransmissionLossRadialChanged, TransmissionLossField.Radials[_selectedRadial - 1]);
                MediatorMessage.Send(MediatorMessage.SetSelectedRadialBearing, TransmissionLossField.Radials[_selectedRadial - 1].BearingFromSource);
                
                //TransmissionLossRadialViewModel.TransmissionLossRadial = TransmissionLossField.Radials[_selectedRadial - 1];
            }
        }
        int _selectedRadial;
        static readonly PropertyChangedEventArgs SelectedRadialChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.SelectedRadial);
        #endregion

        #region public int DepthCount { get; set; }

        public int DepthCount
        {
            get { return TransmissionLossField != null ? TransmissionLossField.Depths.Length : 0; }
            
        }
        static readonly PropertyChangedEventArgs DepthCountChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.DepthCount);
        

        #endregion

        #region public int DepthStep { get; set; }

        public int DepthStep
        {
            get { return TransmissionLossField!=null ? (int)(TransmissionLossField.Depths[1]-TransmissionLossField.Depths[0]):0; }
            
        }

        static readonly PropertyChangedEventArgs DepthStepChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.DepthStep);
        

        #endregion

        #region public int SelectedDepth { get; set; }

        public int SelectedDepth
        {
            get { return _selectedDepth; }
            set
            {
               // if (_selectedDepth == value) return;
                _selectedDepth = value;
                NotifyPropertyChanged(SelectedDepthChangedEventArgs);
                DepthSlice = new TransmissionLossFieldSlice(_transmissionLossField, LookupInfo,
                                                            TransmissionLossFieldSlice.SliceType.Depth, _selectedDepth);
            }
        }

        static readonly PropertyChangedEventArgs SelectedDepthChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.SelectedDepth);
        int _selectedDepth;

        #endregion

        #region public TwoDimensionColorMapViewModel DepthView { get; set; }

        public TwoDimensionColorMapViewModel DepthView
        {
            get { return _depthView ?? (_depthView = new TwoDimensionColorMapViewModel(_viewAwareStatus)); }
            set
            {
                if (_depthView == value) return;
                _depthView = value;
                NotifyPropertyChanged(DepthViewChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs DepthViewChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.DepthView);
        private TwoDimensionColorMapViewModel _depthView;

        #endregion
        
        #region public TwoDimensionColorMapViewModel MeanView { get; set; }

        public TwoDimensionColorMapViewModel MeanView
        {
            get { return _meanView ?? (_meanView = new TwoDimensionColorMapViewModel(_viewAwareStatus)); }
            set
            {
                if (_meanView == value) return;
                _meanView = value;
                NotifyPropertyChanged(MeanViewChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MeanViewChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.MeanView);
        TwoDimensionColorMapViewModel _meanView;

        #endregion

        #region public TwoDimensionColorMapViewModel MaxView { get; set; }

        public TwoDimensionColorMapViewModel MaxView
        {
            get { return _maxView ?? (_maxView = new TwoDimensionColorMapViewModel(_viewAwareStatus)); }
            set
            {
                if (_maxView == value) return;
                _maxView = value;
                NotifyPropertyChanged(MaxViewChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MaxViewChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.MaxView);
        TwoDimensionColorMapViewModel _maxView;

        #endregion

        #region public TwoDimensionColorMapViewModel MinView { get; set; }

        public TwoDimensionColorMapViewModel MinView
        {
            get { return _minView ?? (_minView = new TwoDimensionColorMapViewModel(_viewAwareStatus)); }
            set
            {
                if (_minView == value) return;
                _minView = value;
                NotifyPropertyChanged(MinViewChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MinViewChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.MinView);
        TwoDimensionColorMapViewModel _minView;

        #endregion

        
        #region public TransmissionLossFieldSlice DepthSlice { get; set; }

        public TransmissionLossFieldSlice DepthSlice
        {
            get { return _depthSlice; }
            set
            {
                if (_depthSlice == value) return;
                _depthSlice = value;
                NotifyPropertyChanged(DepthSliceChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs DepthSliceChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.DepthSlice);
        private TransmissionLossFieldSlice _depthSlice;

        #endregion

        #region public TransmissionLossFieldSlice MeanSlice { get; set; }

        public TransmissionLossFieldSlice MeanSlice
        {
            get { return _meanSlice; }
            set
            {
                if (_meanSlice == value) return;
                _meanSlice = value;
                NotifyPropertyChanged(MeanSliceChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs MeanSliceChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.MeanSlice);
        private TransmissionLossFieldSlice _meanSlice;

        #endregion

        #region public TransmissionLossFieldSlice MaxSlice { get; set; }

        public TransmissionLossFieldSlice MaxSlice
        {
            get { return _maxSlice; }
            set
            {
                if (_maxSlice == value) return;
                _maxSlice = value;
                NotifyPropertyChanged(MaxSliceChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs MaxSliceChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.MaxSlice);
        private TransmissionLossFieldSlice _maxSlice;

        #endregion

        #region public TransmissionLossFieldSlice MinSlice { get; set; }

        public TransmissionLossFieldSlice MinSlice
        {
            get { return _minSlice; }
            set
            {
                if (_minSlice == value) return;
                _minSlice = value;
                NotifyPropertyChanged(MinSliceChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs MinSliceChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.MinSlice);
        private TransmissionLossFieldSlice _minSlice;

        #endregion


        [MediatorMessageSink(MediatorMessage.SetSelectedRadialBearing)]
        void SetSelectedRadialBearing(double selectedRadialBearing) { SelectedRadialBearing = selectedRadialBearing; }

        [MediatorMessageSink(MediatorMessage.SaveRadialAsCSV)]
        void SaveRadialAsCSV(string fileName) { TransmissionLossField.Radials[SelectedRadial-1].SaveAsCSV(fileName, TransmissionLossField); }

        [MediatorMessageSink(MediatorMessage.TransmissionLossFieldChanged)]
        void TransmissionLossFieldChanged(TransmissionLossField transmissionLossField)
        {
            if (_iAmInitialized)
            {
                Debug.WriteLine("TransmissionLossFieldViewModel: Initializing transmission loss field");
                TransmissionLossField = transmissionLossField;
            }
            else
            {
                Debug.WriteLine("TransmissionLossFieldViewModel: Deferring initialization of transmission loss field");
                _tempField = transmissionLossField;
            }
        }

        [MediatorMessageSink(MediatorMessage.TransmissionLossFieldViewInitialized)]
        void TransmissionLossFieldViewInitialized(bool dummy)
        {
            _iAmInitialized = true;
            if (_tempField != null)
            {
                TransmissionLossField = _tempField;
                Debug.WriteLine("TransmissionLossFieldViewModel: Deferred initialization of transmission loss field completed");
            }
        }

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
    }

}
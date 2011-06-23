using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
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
                NotifyPropertyChanged(SliceViewChangedEventArgs);
                NotifyPropertyChanged(MeanViewChangedEventArgs);
                NotifyPropertyChanged(MaxViewChangedEventArgs);
                NotifyPropertyChanged(MinViewChangedEventArgs);

                //generate TLFslices. 
                CalculateMaxes();
            }
        }

        private void CalculateMaxes()
        {
            throw new NotImplementedException();
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
                //todo
                SetSelectedDepth(_selectedDepth-1);
            }
        }

        static readonly PropertyChangedEventArgs SelectedDepthChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.SelectedDepth);
        int _selectedDepth;

        #endregion

        #region public TwoDimensionColorMapViewModel SliceView { get; set; }

        public TwoDimensionColorMapViewModel SliceView
        {
            get { return _sliceView ?? (_sliceView = new TwoDimensionColorMapViewModel(_viewAwareStatus)); }
            set
            {
                if (_sliceView == value) return;
                _sliceView = value;
                NotifyPropertyChanged(SliceViewChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SliceViewChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.SliceView);
        TwoDimensionColorMapViewModel _sliceView;

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

        public TransmissionLossSlice.TransmissionLossFieldSlice DepthSlice
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
        private TransmissionLossSlice.TransmissionLossFieldSlice _depthSlice;

        #endregion

        #region public TransmissionLossFieldSlice MeanSlice { get; set; }

        public TransmissionLossSlice.TransmissionLossFieldSlice MeanSlice
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
        private TransmissionLossSlice.TransmissionLossFieldSlice _meanSlice;

        #endregion

        #region public TransmissionLossFieldSlice MaxSlice { get; set; }

        public TransmissionLossSlice.TransmissionLossFieldSlice MaxSlice
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
        private TransmissionLossSlice.TransmissionLossFieldSlice _maxSlice;

        #endregion

        #region public TransmissionLossFieldSlice MinSlice { get; set; }

        public TransmissionLossSlice.TransmissionLossFieldSlice MinSlice
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
        private TransmissionLossSlice.TransmissionLossFieldSlice _minSlice;

        #endregion



#if false
        //protected HorizontalWeightInfo InterpolateToPoint(int xIndex, int yIndex)
        protected void InterpolateToPoint(int xIndex, int yIndex)
        {
            int xSize, ySize, xCoord, yCoord;
            double Bearing_Degrees;
            int Range, StartTransectNumber, EndTransectNumber;
            double OutOfBeamArcCenter_Degrees;
            double OutOfBeamArcHalfAngle_Degrees;
            double StartBearing, EndBearing;
            // HorizontalWeightInfo Results = new HorizontalWeightInfo();

            OutOfBeamArcCenter_Degrees = (this.soundSource.Bearing_degrees + 180) % 360;
            if (this.soundSource.BeamHorizontalWidthHalfAngle_degrees >= 180)
                OutOfBeamArcHalfAngle_Degrees = 0;
            else
                OutOfBeamArcHalfAngle_Degrees = 180.0 - (this.soundSource.BeamHorizontalWidthHalfAngle_degrees);

            xSize = (2 * BoundingBoxX);
            ySize = (2 * BoundingBoxY);

            xCoord = xIndex - BoundingBoxX;
            yCoord = yIndex - BoundingBoxY;
            Bearing_Degrees = Math.Atan2(xCoord, yCoord) * RadiansToDegrees;
            if (Bearing_Degrees < 0)
                Bearing_Degrees += 360;
            Bearing_Degrees %= 360;

            Range = (int)Math.Round(Math.Sqrt((xCoord * xCoord) + (yCoord * yCoord)));

            // Set default values for the weight array
            Results.RangeIndex = Range;
            Results.SourceRadialIndex = -1;

            // If the current point is not within the "out of beam" area, in other words, if it's within the sonar beam
            // at least in an angular sense
            if (!PlaneGeometry.IsWithinArc(Bearing_Degrees, OutOfBeamArcCenter_Degrees, OutOfBeamArcHalfAngle_Degrees))
            {
                // The current point is not out of the sonar beam, unless it's beyond the max range we're computing
                for (int TransectNumber = 0; TransectNumber < soundSource.TransectCount; TransectNumber++)
                {
                    if (TransectNumber < (soundSource.TransectCount - 1))
                    {
                        StartTransectNumber = TransectNumber;
                        EndTransectNumber = TransectNumber + 1;
                    }
                    else
                    {
                        if (OutOfBeamArcHalfAngle_Degrees == 0)
                        {
                            StartTransectNumber = TransectNumber;
                            EndTransectNumber = 0;
                        }
                        else
                            break;
                    }
                    StartBearing = soundSource.TransectProperties[StartTransectNumber].Bearing_degrees;
                    EndBearing = soundSource.TransectProperties[EndTransectNumber].Bearing_degrees;
                    if (PlaneGeometry.IsWithinArcEndpoints(Bearing_Degrees, StartBearing, EndBearing))
                    {
                        int MaxRange = (int)Math.Max(soundSource.TransmissionLossVertical[StartTransectNumber].Data.GetLength(0), soundSource.TransmissionLossVertical[EndTransectNumber].Data.GetLength(0));
                        double TransectAngle_Degrees, CurBearingPercentAngularDistance;

                        if (Range < MaxRange)
                        {
                            TransectAngle_Degrees = PlaneGeometry.AngularDistance_Degrees(StartBearing, EndBearing);
                            CurBearingPercentAngularDistance = PlaneGeometry.AngularDistance_Degrees(StartBearing, Bearing_Degrees) / TransectAngle_Degrees;
                            if (CurBearingPercentAngularDistance <= 0.5)
                                Results.SourceRadialIndex = StartTransectNumber;
                            else
                                Results.SourceRadialIndex = EndTransectNumber;
                            Results.RangeIndex = Range;
                            return Results;
                        } // if (Range < MaxRange)
                        else
                        {
                            // If it's out of the sonar beam because the range to source is greater than max range
                            Results.RangeIndex = Range;
                            Results.SourceRadialIndex = -1;
                            return Results;
                        } // if (Range < MaxRange)
                    } // if (IsWithinArcEndpoints(Bearing_Degrees, StartTransect.Bearing, EndTransect.Bearing))
                } // for (int TransectNumber = 0; TransectNumber < (soundSource.TransectNumber - 1); TransectNumber++)
            } // if (IsWithinArc(Bearing_Degrees, OutOfBeamArcCenter_Degrees, OutOfBeamArcHalfAngle_Degrees))
            return Results;
        } // private HorizontalWeightInfo InterpolateToPoint(int xIndex, int yIndex)  
#endif

        [MediatorMessageSink(MediatorMessage.SetSelectedRadialBearing)]
        void SetSelectedRadialBearing(double selectedRadialBearing) { SelectedRadialBearing = selectedRadialBearing; }

        [MediatorMessageSink(MediatorMessage.SetSelectedDepth)]
        void SetSelectedDepth(int selectedDepth) { SelectedDepth = selectedDepth; }

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
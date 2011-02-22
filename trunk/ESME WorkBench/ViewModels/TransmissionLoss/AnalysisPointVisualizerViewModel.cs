﻿using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Windows;
using System.Windows.Input;
using System.Windows.Threading;
using Cinch;
using ESME.TransmissionLoss;
using ESMEWorkBench.Views;
using HRC.Services;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    class AnalysisPointVisualizerViewModel: ViewModelBase, IViewStatusAwareInjectionAware
    {
        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;
        readonly IHRCSaveFileService _saveFileService;
        bool _iAmInitialized;
        readonly AnalysisPoint _tempAnalysisPoint;

        #region public constructor

        public AnalysisPointVisualizerViewModel(AnalysisPoint analysisPoint, IHRCSaveFileService saveFileService)
        {
            RegisterMediator();
            _saveFileService = saveFileService;
            
            if (_iAmInitialized)
            {
                Debug.WriteLine("AnalysisPointVisualizerViewModel: Initializing analysis point");
                MediatorMessage.Send(MediatorMessage.AnalysisPointChanged, analysisPoint);
                TransmissionLossFieldChanged(analysisPoint.TransmissionLossFields[0]);
                
            }
            else
            {
                Debug.WriteLine("AnalysisPointVisualizerViewModel: Deferring initialization of analysis point");
                _tempAnalysisPoint = analysisPoint;
            }
        }

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

        #region public string SelectedTransmissionLossFieldName { get; set; }

        public string SelectedTransmissionLossFieldName
        {
            get { return _selectedTransmissionLossFieldName; }
            set
            {
                if (_selectedTransmissionLossFieldName == value) return;
                _selectedTransmissionLossFieldName = value;
                NotifyPropertyChanged(SelectedTransmissionLossFieldNameChangedEventArgs);
                
                
                
            }
        }

        static readonly PropertyChangedEventArgs SelectedTransmissionLossFieldNameChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointVisualizerViewModel>(x => x.SelectedTransmissionLossFieldName);
        string _selectedTransmissionLossFieldName;

        #endregion

        #region public string  OutputFileName { get; set; }

        public string  OutputFileName
        {
            get
            {
                //update the default ouput file string
                //todo:possible race condition ?
                lock (this)
                {
                    if (SelectedTransmissionLossFieldName == null) return null;
                    var fieldName = SelectedTransmissionLossFieldName.Replace('|', ' ');
                    return Path.Combine(Properties.Settings.Default.ExperimentReportDirectory, fieldName + string.Format(" radial {0} degrees", SelectedRadialBearing));
                }


            }

        }
        
        #endregion
        
        #region CloseWindowCommand

        SimpleCommand<object, object> _closeWindow;

        public SimpleCommand<object, object> CloseWindowCommand
        {
            get { return _closeWindow ?? (_closeWindow = new SimpleCommand<object, object>(delegate
                                                                                           {
                                                                                               ((AnalysisPointVisualizerView)_viewAwareStatus.View).Close();

                                                                                           })); }
        }

        #endregion

        #region SaveAsCommand

        SimpleCommand<object, object> _saveAs;

        public SimpleCommand<object, object> SaveAsCommand
        {
            get
            {
                return _saveAs ?? (_saveAs = new SimpleCommand<object, object>(delegate
                {
                    
                    _saveFileService.Filter = "Portable Network Graphics (*.png)|*.png| JPEG (*.jpeg)|*.jpg|Bitmap (*.bmp)|*.bmp";
                    _saveFileService.OverwritePrompt = true;
                    //if (OutputFileName == null) return;
                    _saveFileService.FileName = OutputFileName;
                   
                    var result = _saveFileService.ShowDialog((Window)_viewAwareStatus.View);
                    if (result.HasValue && result.Value)
                    {
                        Properties.Settings.Default.LastImageExportFileDirectory = Path.GetDirectoryName(_saveFileService.FileName);
                        MediatorMessage.Send(MediatorMessage.SaveRadialBitmap,_saveFileService.FileName);
                    }
                    //MediatorMessage.Send(MediatorMessage.ResetSelectedField, true);
                    Keyboard.Focus((Window)_viewAwareStatus.View);
                }));
            }
        }

        #endregion

        #region ExportAsCommand

        public SimpleCommand<object, object> ExportAsCommand
        {
            get
            {
                return _exportAs ?? (_exportAs = new SimpleCommand<object, object>(delegate
                {
                    _saveFileService.Filter = "Comma-Separated Value (*.csv)|*.csv";
                    _saveFileService.OverwritePrompt = true;
                    _saveFileService.FileName = OutputFileName;

                    var result = _saveFileService.ShowDialog((Window)_viewAwareStatus.View);
                    if (result.HasValue && result.Value)
                    {
                        Properties.Settings.Default.LastCSVExportFileDirectory = Path.GetDirectoryName(_saveFileService.FileName);
                        MediatorMessage.Send(MediatorMessage.SaveRadialAsCSV, _saveFileService.FileName);
                    }
                    //MediatorMessage.Send(MediatorMessage.ResetSelectedField,true);
                    Keyboard.Focus((Window)_viewAwareStatus.View);
                }));
            }
        }

        SimpleCommand<object, object> _exportAs;

        #endregion

        [MediatorMessageSink(MediatorMessage.SetSelectedRadialBearing)]
        void SetSelectedRadialBearing(double selectedRadialBearing) { SelectedRadialBearing = selectedRadialBearing; }

        [MediatorMessageSink(MediatorMessage.TransmissionLossFieldChanged)]
        void TransmissionLossFieldChanged(TransmissionLossField transmissionLossField) { lock(this) SelectedTransmissionLossFieldName = transmissionLossField.Name; }

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService) 
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
            _iAmInitialized = true;

            if (_tempAnalysisPoint != null)
            {
                TransmissionLossFieldChanged(_tempAnalysisPoint.TransmissionLossFields[0]);
                MediatorMessage.Send(MediatorMessage.AnalysisPointChanged, _tempAnalysisPoint);
                Debug.WriteLine("AnalysisPointVisualizerViewModel: Deferred initialization of analysis point completed");
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
                Debug.WriteLine("***********\nAnalysisPointVisualizerViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
        }
    }
}
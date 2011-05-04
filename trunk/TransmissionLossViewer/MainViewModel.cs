using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Input;
using System.Windows.Threading;
using Cinch;
using ESME;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.CASS;
using ESME.Views.Services;
using ESME.Views.TransmissionLossViewer;
using HRC.Services;
using MEFedMVVM.ViewModelLocator;


namespace TransmissionLossViewer
{
    [ExportViewModel("MainViewModel")]
    class MainViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {

        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;
        readonly IHRCSaveFileService _saveFileService;
        readonly IHRCOpenFileService _openFileService;
        readonly IViewParameterService _viewParameterService;
        bool _iAmInitialized;
        readonly AnalysisPoint _tempAnalysisPoint;

        #region public constructor
        [ImportingConstructor]
        public MainViewModel(IHRCSaveFileService saveFileService, IHRCOpenFileService openFileService, IViewParameterService viewParameterService)
        {
            RegisterMediator();
            _saveFileService = saveFileService;
            _openFileService = openFileService;
            _viewParameterService = viewParameterService;
            _viewParameterService.TransmissionLayersWidth = Properties.Settings.Default.TransmissionLayersWidth;
            _viewParameterService.PropertyChanged += (s, e) =>
                                                     {
                                                         switch (e.PropertyName)
                                                         {
                                                             case "TransmissionLayersWidth":
                                                                 Properties.Settings.Default.TransmissionLayersWidth = _viewParameterService.TransmissionLayersWidth;
                                                                 break;
                                                         }
                                                     };
            
#if false

            if (_iAmInitialized)
            {
                Debug.WriteLine("MainViewModel: Initializing analysis point");
                MediatorMessage.Send(MediatorMessage.AnalysisPointChanged, analysisPoint);
                TransmissionLossFieldChanged(analysisPoint.TransmissionLossFields[0]);
                
            }
            else
            {
                Debug.WriteLine("MainViewModel: Deferring initialization of analysis point");
                _tempAnalysisPoint = analysisPoint;
            }
#endif
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

        static readonly PropertyChangedEventArgs SelectedTransmissionLossFieldNameChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedTransmissionLossFieldName);
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

        #region public double TransmissionLayersWidth { get; set; }

        public double TransmissionLayersWidth
        {
            get { return Properties.Settings.Default.TransmissionLayersWidth; }
            set
            {
                Properties.Settings.Default.TransmissionLayersWidth = value;
                NotifyPropertyChanged(TransmissionLayersWidthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLayersWidthChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.TransmissionLayersWidth);

        #endregion

        #region ViewClosingCommand

        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand
        {
            get
            {
                return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(vcArgs =>
                {
                    var ea = (CancelEventArgs)vcArgs.EventArgs;
                    Properties.Settings.Default.Save();
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;

        #endregion
        
        #region CloseWindowCommand

        SimpleCommand<object, object> _closeWindow;

        public SimpleCommand<object, object> CloseWindowCommand
        {
            get { return _closeWindow ?? (_closeWindow = new SimpleCommand<object, object>(delegate
                                                                                           {
                                                                                               ((MainView)_viewAwareStatus.View).Close();

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

        #region OpenCommand

        public SimpleCommand<object, object> OpenCommand
        {
            get { return _open ?? (_open = new SimpleCommand<object, object>(
                delegate
                {
                    _openFileService.Filter = "CASS Output (*.bin)|*.bin |";
                    _openFileService.InitialDirectory = Properties.Settings.Default.ExperimentReportDirectory;
                    _openFileService.Title = "Select a Transmission Loss file to view";

                    var result = _openFileService.ShowDialog((Window) _viewAwareStatus.View);
                    if (result.HasValue && result.Value)
                    {
                        CASSOutput.Load(result.ToString(), false);
                    }
                })); }
        }

        SimpleCommand<object, object> _open;

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
                Debug.WriteLine("MainViewModel: Deferred initialization of analysis point completed");
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
                Debug.WriteLine("***********\nMainViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
        }
    }
}

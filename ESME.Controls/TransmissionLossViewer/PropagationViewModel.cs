using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Windows;
using System.Windows.Input;
using System.Windows.Threading;
using Cinch;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.CASS;
using ESME.Views.Services;
using HRC.Services;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.TransmissionLossViewer
{
    [ExportViewModel("PropagationViewModel")]
    public class PropagationViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;
        readonly IHRCSaveFileService _saveFileService;
        readonly IHRCOpenFileService _openFileService;
        readonly IViewParameterService _viewParameterService;
        readonly IMessageBoxService _messageBoxService;
        readonly IUIVisualizerService _visualizerService;
        //bool _iAmInitialized;
        //readonly AnalysisPoint _tempAnalysisPoint;

        #region public constructor
        [ImportingConstructor]
        public PropagationViewModel(CASSOutput cassOutput, IHRCSaveFileService saveFileService, IHRCOpenFileService openFileService, IMessageBoxService messageBoxService, IUIVisualizerService visualizerService)
        {
            RegisterMediator();
            _saveFileService = saveFileService;
            _openFileService = openFileService;
            _messageBoxService = messageBoxService;
            _visualizerService = visualizerService;
            _viewParameterService = new ViewParameterService
            {TransmissionLayersWidth = Properties.Settings.Default.TransmissionLayersWidth};
            _viewParameterService.PropertyChanged += (s, e) =>
            {
                switch (e.PropertyName)
                {
                    case "TransmissionLayersWidth":
                        Properties.Settings.Default.TransmissionLayersWidth = _viewParameterService.TransmissionLayersWidth;
                        break;
                }
            };
            CASSOutput = cassOutput;
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

        static readonly PropertyChangedEventArgs SelectedTransmissionLossFieldNameChangedEventArgs = ObservableHelper.CreateArgs<PropagationViewModel>(x => x.SelectedTransmissionLossFieldName);
        string _selectedTransmissionLossFieldName;

        #endregion

        #region public string  OutputFileName { get; set; }

        public string OutputFileName
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

        static readonly PropertyChangedEventArgs TransmissionLayersWidthChangedEventArgs = ObservableHelper.CreateArgs<PropagationViewModel>(x => x.TransmissionLayersWidth);

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
            get
            {
                return _closeWindow ?? (_closeWindow = new SimpleCommand<object, object>(delegate
                {
                    ((PropagationView)_viewAwareStatus.View).Close();

                }));
            }
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
                        MediatorMessage.Send(MediatorMessage.SaveRadialBitmap, _saveFileService.FileName);
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
        void TransmissionLossFieldChanged(TransmissionLossField transmissionLossField) { lock (this) SelectedTransmissionLossFieldName = transmissionLossField.Name; }

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
            //_iAmInitialized = true;
            CASSOutput.Load();
            var tlf = TransmissionLossField.FromCASS(CASSOutput);
            MediatorMessage.Send(MediatorMessage.TransmissionLossFieldChanged, tlf);
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

        #region public CASSOutput CASSOutput { get; set; }

        public CASSOutput CASSOutput
        {
            get { return _cassOutput; }
            set
            {
                if (_cassOutput == value) return;
                _cassOutput = value;
                NotifyPropertyChanged(CASSOutputChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CASSOutputChangedEventArgs = ObservableHelper.CreateArgs<PropagationViewModel>(x => x.CASSOutput);
        CASSOutput _cassOutput;

        #endregion

#if false
        void OpenCASSFile(string filename)
        {
            CASSOutput = CASSOutput.FromBinaryFile(filename, false);
            var tlf = TransmissionLossField.FromCASS(CASSOutput);
            MediatorMessage.Send(MediatorMessage.TransmissionLossFieldChanged, tlf);
        } 
#endif

    }
}

using System.ComponentModel;
using System.IO;
using System.Windows;
using Cinch;
using ESME.TransmissionLoss;
using ESMEWorkBench.Properties;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class TransmissionLossFieldViewModel : ViewModelBase
    {
        private readonly ISaveFileService _saveFileService;
        private IUIVisualizerService _visualizerService;

        public TransmissionLossFieldViewModel(string fileName, ISaveFileService saveFileService, IUIVisualizerService visualizerService)
        {
            TransmissionLossField = TransmissionLossField.Load(fileName);
            ColorMapViewModel = ColorMapViewModel.Default;
            _saveFileService = saveFileService;
            _visualizerService = visualizerService;
            SelectedRadial = 1;
        }

        public TransmissionLossFieldViewModel(TransmissionLossField transmissionLossField, ISaveFileService saveFileService, IUIVisualizerService visualizerService)
        {
            TransmissionLossField = transmissionLossField;
            ColorMapViewModel = ColorMapViewModel.Default;
            _saveFileService = saveFileService;
            _visualizerService = visualizerService;

            SelectedRadial = 1;
        }

        public TransmissionLossField TransmissionLossField { get; private set; }

        public ColorMapViewModel ColorMapViewModel { get; private set; }

        public int RadialCount { get { return TransmissionLossField.Radials.Length; } }

        int _selectedRadial;
        static readonly PropertyChangedEventArgs SelectedRadialChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.SelectedRadial);
        public int SelectedRadial
        { 
            get { return _selectedRadial; } 
            set
            {
                if (value == _selectedRadial) return;
                _selectedRadial = value;
                NotifyPropertyChanged(SelectedRadialChangedEventArgs);
                TransmissionLossRadialViewModel = new TransmissionLossRadialViewModel(TransmissionLossField.Radials[_selectedRadial - 1], ColorMapViewModel);
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLossRadialViewModelChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.TransmissionLossRadialViewModel);
        TransmissionLossRadialViewModel _transmissionLossRadialViewModel;
        public TransmissionLossRadialViewModel TransmissionLossRadialViewModel {
            get { return _transmissionLossRadialViewModel; } 
            private set
            {
                if (value == _transmissionLossRadialViewModel) return;
                _transmissionLossRadialViewModel = value;
                NotifyPropertyChanged(TransmissionLossRadialViewModelChangedEventArgs);
                ColorMapViewModel.MaxValue = TransmissionLossRadialViewModel.TransmissionLossRadial.StatMax;
                ColorMapViewModel.MinValue = TransmissionLossRadialViewModel.TransmissionLossRadial.StatMin;
            }
        }

        #region SaveAsCommand

        public SimpleCommand<object, object> SaveAsCommand
        {
            get
            {
                return _saveAs ??
                       (_saveAs =
                        new SimpleCommand<object, object>(
                            delegate
                            {
                                _saveFileService.Filter = "Portable Network Graphics (*.png)|(*.png)| JPEG (*.jpg)|(*.jpg)|Bitmap (*.bmp)|(*.bmp)";
                                _saveFileService.OverwritePrompt = true;
                                //_saveFileService.ShowDialog()
                                
                            }));
            }
        }
        

        private SimpleCommand<object, object> _saveAs;

        #endregion


    }
}
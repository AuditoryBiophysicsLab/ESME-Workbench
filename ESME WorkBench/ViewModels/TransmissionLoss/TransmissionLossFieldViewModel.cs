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
        public TransmissionLossFieldViewModel(string fileName, ISaveFileService saveFileService)
        {
            TransmissionLossField = TransmissionLossField.Load(fileName);
            ColorMapViewModel = ColorMapViewModel.Default;
            _saveFileService = saveFileService;
            SelectedRadial = 1;
        }

        public TransmissionLossFieldViewModel(TransmissionLossField transmissionLossField, ISaveFileService saveFileService)
        {
            TransmissionLossField = transmissionLossField;
            ColorMapViewModel = ColorMapViewModel.Default;
            _saveFileService = saveFileService;

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
                                _saveFileService.Filter = "Portable Network Graphics (PNG) (*.png)|All files (*.*)|*.*";
                                _saveFileService.OverwritePrompt = true;
                            }));
            }
        }
        

        private SimpleCommand<object, object> _saveAs;

        #endregion


    }
}
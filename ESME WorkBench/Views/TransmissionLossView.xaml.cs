using System.IO;
using System.Windows;
using Cinch;
using ESMEWorkBench.Properties;

namespace ESMEWorkBench.Views
{
    /// <summary>
    ///   Interaction logic for TransmissionLossView.xaml
    /// </summary>
    [PopupNameToViewLookupKeyMetadata("TransmissionLossView", typeof (TransmissionLossView))]
    public partial class TransmissionLossView
    {
        readonly ISaveFileService _saveFileService;
        readonly IViewAwareStatus _viewAwareStatusService;
        readonly IUIVisualizerService _visualizerService;
        public TransmissionLossView() { InitializeComponent(); }

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
                                    Mediator.Instance.NotifyColleagues("SaveAsCommandMessage");
                                    SaveRadialBitmap();
                                }));
            }
        }

        public bool SaveRadialBitmap()
        {
            _saveFileService.Filter = "Portable Network Graphics (PNG) (*.png)|*.esme|All files (*.*)|*.*";
            _saveFileService.OverwritePrompt = true;
            _saveFileService.InitialDirectory = Settings.Default.LastExperimentFileDirectory;
            var result = _saveFileService.ShowDialog((Window) _viewAwareStatusService.View);
            if ((!result.HasValue) || (!result.Value)) return false;
            Settings.Default.LastExperimentFileDirectory = Path.GetDirectoryName(_saveFileService.FileName);
            return true;
        }

        private SimpleCommand<object, object> _saveAs;

        #endregion
    }
}
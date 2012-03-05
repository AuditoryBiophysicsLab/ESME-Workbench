using System.ComponentModel;
using System.Windows;
using Cinch;

namespace ESMEWorkbench.ViewModels.Main
{
    public class PleaseWaitViewModel : ViewModelBase
    {
        readonly Window _parentWindow;
        Window _myWindow;
        readonly IUIVisualizerService _visualizerService;
        public PleaseWaitViewModel(Window parentWindow, IUIVisualizerService visualizerService) 
        {
            _parentWindow = parentWindow;
            _visualizerService = visualizerService;
        }

        #region public string Message { get; set; }

        public string Message
        {
            get { return _message; }
            set
            {
                if (_message == value) return;
                _message = value;
                NotifyPropertyChanged(MessageChangedEventArgs);
                Show();
            }
        }

        static readonly PropertyChangedEventArgs MessageChangedEventArgs = ObservableHelper.CreateArgs<PleaseWaitViewModel>(x => x.Message);
        string _message;

        #endregion

        public void Show()
        {
            if (_myWindow == null)
            {
                _myWindow = _visualizerService.ShowWindow("PleaseWaitView", this, true, null);
                _myWindow.LayoutUpdated += (s, e) =>
                {
                    _myWindow.Top = _parentWindow.Top + ((_parentWindow.Height - _myWindow.Height) / 2);
                    _myWindow.Left = _parentWindow.Left + ((_parentWindow.Width - _myWindow.Width) / 2);
                };
            }

            _myWindow.Top = _parentWindow.Top + ((_parentWindow.Height - _myWindow.Height) / 2);
            _myWindow.Left = _parentWindow.Left + ((_parentWindow.Width - _myWindow.Width) / 2);
        
            _myWindow.Show();
        }

        public void Hide()
        {
            _myWindow.Hide();
        }
    }
}

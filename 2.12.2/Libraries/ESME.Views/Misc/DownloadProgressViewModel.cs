using System.Net;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Misc
{
    /// <summary>
    /// To create and show the view as a dialog:
    /// var vm = new DownloadProgressViewModel {...};
    /// var result = _visualizerService.ShowDialog("DownloadProgressView", vm);
    /// if ((!result.HasValue) || (!result.Value)) return;
    /// 
    /// To create and show the view as a window:
    /// var vm = new DownloadProgressViewModel {...};
    /// var window = _visualizerService.ShowWindow("DownloadProgressView", vm);
    /// </summary>
    public class DownloadProgressViewModel : ViewModelBase
    {
        public int ProgressPercent { get; set; }
        public string WindowTitle { get; set; }
        public string Message { get; set; }
        public WebClient WebClient { get; set; }
        public bool IsCanceled { get; private set; }

        #region CancelCommand
        public SimpleCommand<object, EventToCommandArgs> CancelCommand
        {
            get { return _cancel ?? (_cancel = new SimpleCommand<object, EventToCommandArgs>(CancelHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _cancel;

        void CancelHandler(EventToCommandArgs args)
        {
            WebClient.CancelAsync();
            IsCanceled = true;
        }
        #endregion
    }
}

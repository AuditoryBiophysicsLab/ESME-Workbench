using System.IO;
using System.Reflection;
using HRC.Services;
using HRC.ViewModels;
using HRC.WPF;

namespace ESMEWorkbench.ViewModels.Main
{
    public class FirstRunQuestionViewModel : ViewModelBase
    {
        public IMessageBoxService MessageBoxService { get; set; }
        #region ShowQuickStartCommand
        public SimpleCommand<object, EventToCommandArgs> ShowQuickStartCommand { get { return _showQuickStart ?? (_showQuickStart = new SimpleCommand<object, EventToCommandArgs>(ShowQuickStartHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _showQuickStart;

        void ShowQuickStartHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter;
            var quickStartFilename = Path.Combine(Path.GetDirectoryName(Assembly.GetEntryAssembly().Location), "ESME Workbench Quick Start.pdf");
            if (File.Exists(quickStartFilename)) System.Diagnostics.Process.Start(quickStartFilename);
            else if (MessageBoxService != null) MessageBoxService.ShowError("The Quick Start Guide was not found!");
        }
        #endregion

        #region OkCommand
        public SimpleCommand<object, EventToCommandArgs> OkCommand { get { return _ok ?? (_ok = new SimpleCommand<object, EventToCommandArgs>(o => CloseDialog(true))); } }
        SimpleCommand<object, EventToCommandArgs> _ok;
        #endregion

        #region CancelCommand
        public SimpleCommand<object, EventToCommandArgs> CancelCommand { get { return _cancel ?? (_cancel = new SimpleCommand<object, EventToCommandArgs>(o => CloseDialog(false))); } }
        SimpleCommand<object, EventToCommandArgs> _cancel;
        #endregion
    }
    public class FirstRunProgressViewModel : ViewModelBase
    {
        public FirstRunProgressViewModel() { CancelText = "Cancel"; IsCancelEnabled = true; }
        public string ProgressMessage { get; set; }
        public int ItemCount { get; set; }
        public int CurrentItem { get; set; }
        public bool IsCanceled { get; set; }

        public string CancelText { get; set; }
        public bool IsCancelEnabled { get; set; }

        #region CancelCommand
        public SimpleCommand<object, EventToCommandArgs> CancelCommand { get { return _cancel ?? (_cancel = new SimpleCommand<object, EventToCommandArgs>(CancelHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _cancel;

        void CancelHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter;
            IsCanceled = true;
            CancelText = "Canceling...";
            IsCancelEnabled = false;
        }
        #endregion
    }
}

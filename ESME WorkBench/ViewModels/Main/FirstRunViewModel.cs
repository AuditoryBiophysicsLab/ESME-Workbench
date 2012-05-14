using HRC.ViewModels;
using HRC.WPF;
using HRC.Aspects;

namespace ESMEWorkbench.ViewModels.Main
{
    public class FirstRunQuestionViewModel : ViewModelBase
    {
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

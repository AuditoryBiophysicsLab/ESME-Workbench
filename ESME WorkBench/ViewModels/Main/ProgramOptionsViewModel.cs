using System;
using Cinch;
using ESMEWorkBench.Data;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.Main
{
    public class ProgramOptionsViewModel : ViewModelBase, IDesignTimeAware
    {
        public ProgramOptionsViewModel()
        {
            AppSettings = Globals.AppSettings;

            OkCommand = new SimpleCommand<object, object>(delegate
            {
                AppSettings.Save();
                CloseActivePopUpCommand.Execute(true);
            });
            CancelCommand = new SimpleCommand<object, object>(delegate
            {
                AppSettings.Reload();
                CloseActivePopUpCommand.Execute(false);
            });
        }

        public void DesignTimeInitialization() { AppSettings = AppSettings.Load(); }

        public AppSettings AppSettings { get; private set; }
        
        public SimpleCommand<Object, Object> OkCommand { get; private set; }
        public SimpleCommand<Object, Object> CancelCommand { get; private set; }
    }
}
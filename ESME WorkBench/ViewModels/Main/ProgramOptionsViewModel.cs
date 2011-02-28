using System;
using Cinch;
using ESME.Data;
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
                AppSettings.Save(null);
                CloseActivePopUpCommand.Execute(true);
            });
            CancelCommand = new SimpleCommand<object, object>(delegate
            {
                AppSettings.Reload(null);
                CloseActivePopUpCommand.Execute(false);
            });
        }

        public void DesignTimeInitialization() { AppSettings = AppSettings.Load(null); }

        public AppSettings AppSettings { get; private set; }
        
        public SimpleCommand<Object, Object> OkCommand { get; private set; }
        public SimpleCommand<Object, Object> CancelCommand { get; private set; }
    }
}
using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Configuration;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.Main
{
    public class ProgramOptionsViewModel : ViewModelBase, IDesignTimeAware
    {
        readonly IMessageBoxService _messageBoxService;
        readonly IOpenFileService _openFileService;

        public ProgramOptionsViewModel(IMessageBoxService messageBoxService, IOpenFileService openFileService, Data.AppSettings appSettings)
        {
            _messageBoxService = messageBoxService;
            _openFileService = openFileService;
            _appSettings = appSettings;

            //ConfigurationManager.AppSettings[]
            OkCommand = new SimpleCommand<object, object>(delegate
                                                          {
                                                              CloseActivePopUpCommand.Execute(true);
                                                          });
        }

        public Data.AppSettings AppSettings
        {
            get { return _appSettings; }
            set
            {
                if (_appSettings == value) return;
                _appSettings = value;
                NotifyPropertyChanged(AppSettingsChangedEventArgs);
            }
        }

        private Data.AppSettings _appSettings;

        private static readonly PropertyChangedEventArgs AppSettingsChangedEventArgs =
            ObservableHelper.CreateArgs<ProgramOptionsViewModel>(x => x.AppSettings);

        #region IDesignTimeAware Members

        public void DesignTimeInitialization() 
        { 
            throw new NotImplementedException(); 
        }

        #endregion

        public SimpleCommand<Object, Object> OkCommand { get; private set; }
    }
}
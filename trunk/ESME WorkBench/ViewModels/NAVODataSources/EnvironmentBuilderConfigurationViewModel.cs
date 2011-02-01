using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME.Views.EnvironmentBuilder;
using ESMEWorkBench.Data;
using ESMEWorkBench.Properties;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    [ExportViewModel("EnvironmentBuilderConfigurationViewModel")]
    internal class EnvironmentBuilderConfigurationViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;


        #region public AppSettings AppSettings { get; set; }

        public AppSettings AppSettings
        {
            get { return _appSettings; }
            set
            {
                if (_appSettings == value) return;
                _appSettings = value;
                NotifyPropertyChanged(AppSettingsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AppSettingsChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderConfigurationViewModel>(x => x.AppSettings);
        AppSettings _appSettings;

        #endregion

        public EnvironmentBuilderConfigurationViewModel(AppSettings appSettings)
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nEnvironmentBuilderConfigurationViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
            AppSettings = appSettings;
        }

        #region OkCommand

        SimpleCommand<object, object> _ok;

        public SimpleCommand<object, object> OkCommand
        {
            get
            {
                return _ok ?? (_ok = new SimpleCommand<object, object>(delegate
                                                                       {

                                                                           //if all the database locations have been filled in, etc, then the user can click. 

                                                                           return true;// (!string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.GDEMDirectory) && !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.SMGCDirectory) && !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.BSTDirectory) && !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.DBDBDirectory) && !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.GDEMEXEPath) && !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.SMGCEXEPath) && !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.BSTEXEPath) && !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.DBDBEXEPath));

                                                                       }, delegate
                                                                          {
                                                                              //fire off a message, and close the window.
                                                                              AppSettings.Save();
                                                                              MediatorMessage.Send(MediatorMessage.EnvironmentBuilderDatabasesSpecified);
                                                                              ((EnvironmentBuilderConfigurationView)_viewAwareStatus.View).Close();
                                                                          }));
            }
        }

        #endregion

        #region CancelCommand

        public SimpleCommand<object, object> CancelCommand
        {
            get { return _cancel ?? (_cancel = new SimpleCommand<object, object>(delegate { AppSettings.Reload(); })); }
        }

        SimpleCommand<object, object> _cancel;

        #endregion

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
        }
    }
}
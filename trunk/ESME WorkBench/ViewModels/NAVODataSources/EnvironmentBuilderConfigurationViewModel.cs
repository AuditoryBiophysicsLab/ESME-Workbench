using System;
using System.Diagnostics;
using System.IO;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME.Views.EnvironmentBuilder;
using ESMEWorkBench.Properties;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    [ExportViewModel("EnvironmentBuilderConfigurationViewModel")]
    internal class EnvironmentBuilderConfigurationViewModel
    {
        Dispatcher _dispatcher;
        bool _iAmInitialized;
        IViewAwareStatus _viewAwareStatus;
        public EnvironmentBuilderConfigurationViewModel() { RegisterMediator(); }

        #region OkCommand

        SimpleCommand<object, object> _ok;

        public SimpleCommand<object, object> OkCommand
        {
            get
            {
                return _ok ?? (_ok = new SimpleCommand<object, object>(delegate
                                                                       {
                                                                           //if all the database locations have been filled in, etc, then the user can click. 
                                                                           return Directory.Exists(Settings.Default.GDEMDirectory) && Directory.Exists(Settings.Default.SMGCDirectory) && Directory.Exists(Settings.Default.BSTDirectory) && Directory.Exists(Settings.Default.DBDBDirectory) && File.Exists(Settings.Default.GDEMEXEDirectory) && File.Exists(Settings.Default.SMGCEXEDirectory) && File.Exists(Settings.Default.BSTEXEDirectory) && File.Exists(Settings.Default.DBDBEXEDirectory);
                                                                       }, delegate
                                                                          {
                                                                              //fire off a message, and close the window.
                                                                              MediatorMessage.Send(MediatorMessage.EnvironmentBuilderDatabasesSpecified);
                                                                              ((EnvironmentBuilderConfigurationView) _viewAwareStatus.View).Close();
                                                                          }));
            }
        }

        #endregion

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window) _viewAwareStatus.View).Dispatcher;
            _iAmInitialized = true;
        }

        void RegisterMediator()
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
        }
    }
}
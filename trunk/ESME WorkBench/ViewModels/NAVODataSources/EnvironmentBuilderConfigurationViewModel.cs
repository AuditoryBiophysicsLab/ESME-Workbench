using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME.Views.EnvironmentBuilder;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    [ExportViewModel("EnvironmentBuilderConfigurationViewModel")]
    class EnvironmentBuilderConfigurationViewModel
    {
        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;
        bool _iAmInitialized;
        public EnvironmentBuilderConfigurationViewModel()
        {
            RegisterMediator();
        }
        
        #region OkCommand

        public SimpleCommand<object, object> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, object>(delegate
                                                                         {
                                                                             //if all the database locations have been filled in, etc, then the user can click. 
                                                                             return Directory.Exists(Properties.Settings.Default.GDEMDirectory) && Directory.Exists(Properties.Settings.Default.SMGCDirectory) &&Directory.Exists(Properties.Settings.Default.BSTDirectory) &&Directory.Exists(Properties.Settings.Default.DBDBDirectory)
                                                                                    && File.Exists(Properties.Settings.Default.GDEMEXEDirectory) && File.Exists(Properties.Settings.Default.SMGCEXEDirectory) && File.Exists(Properties.Settings.Default.BSTEXEDirectory) && File.Exists(Properties.Settings.Default.DBDBEXEDirectory);
                                                                         },
                                                                         delegate
                                                                         {
                                                                             //fire off a message, and close the window.
                                                                             MediatorMessage.Send(MediatorMessage.EnvironmentBuilderDatabasesSpecified);
                                                                             ((EnvironmentBuilderConfigurationView)_viewAwareStatus.View).Close();
                                                                         }));
            }

        }

        SimpleCommand<object, object> _ok;

        #endregion

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
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

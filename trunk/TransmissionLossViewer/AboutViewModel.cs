using System;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME;
using ESME.Views.Misc;

namespace TransmissionLossViewer
{
    public class AboutViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;
        readonly AssemblyName _assemblyName;
        readonly string _assemblyDescription;
        readonly string _assemblyTitle;

        public AboutViewModel()
        {
            RegisterMediator();
            var assembly = Assembly.GetAssembly(typeof (AboutViewModel));
            _assemblyName = assembly.GetName();
            // assembly description attribute
            _assemblyDescription = ((AssemblyDescriptionAttribute)assembly.GetCustomAttributes(typeof(AssemblyDescriptionAttribute), false)[0]).Description;
            // assembly title attribute
            _assemblyTitle = ((AssemblyTitleAttribute)assembly.GetCustomAttributes(typeof(AssemblyTitleAttribute), false)[0]).Title;
            
            BuildInfo = new ModuleBuildInfoViewModel("Transmission Loss Viewer", BuildInformation.BuildDateTime, BuildInformation.BuildEngineer, BuildInformation.SVNVersion);
        }
        
        public ModuleBuildInfoViewModel BuildInfo { get; private set; }

        #region OkCommand

        public SimpleCommand<object, object> OkCommand
        {
            get
            {
                return _okCommand ?? (_okCommand = new SimpleCommand<object, object>(x => CloseActivePopUpCommand.Execute(true)));
            }
        }

        SimpleCommand<object, object> _okCommand;

        #endregion

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
        }

        void RegisterMediator()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nAboutViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
        }
    }
}
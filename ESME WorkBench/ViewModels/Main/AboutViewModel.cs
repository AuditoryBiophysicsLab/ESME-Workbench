using System;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using HRC.Utility;
using ESME.Views.Misc;

namespace ESMEWorkBench.ViewModels.Main
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
            WorkbenchModuleBuildInfo = new ModuleBuildInfoViewModel("ESME WorkBench", BuildInformation.BuildDateTime, BuildInformation.BuildEngineer, BuildInformation.SVNVersion);
            ESMEModuleBuildInfo = new ModuleBuildInfoViewModel("ESME.dll", ESME.BuildInformation.BuildDateTime, ESME.BuildInformation.BuildEngineer, ESME.BuildInformation.SVNVersion);
            HRCModuleBuildInfo = new ModuleBuildInfoViewModel("HRC.dll", HRC.BuildInformation.BuildDateTime, HRC.BuildInformation.BuildEngineer, HRC.BuildInformation.SVNVersion);
            ViewsModuleBuildInfo = new ModuleBuildInfoViewModel("ESME.Views.dll", ESME.Views.BuildInformation.BuildDateTime, ESME.Views.BuildInformation.BuildEngineer, ESME.Views.BuildInformation.SVNVersion);
            var appDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
            if (string.IsNullOrEmpty(appDir)) return;
            MapDllVersion = Assembly.LoadFile(Path.Combine(appDir, "WpfDesktopEdition.dll")).GetName().Version.ToString();
            MapCoreVersion = Assembly.LoadFile(Path.Combine(appDir, "MapSuiteCore.dll")).GetName().Version.ToString();
            NetTopologyVersion = Assembly.LoadFile(Path.Combine(appDir, "NetTopologySuite.dll")).GetName().Version.ToString();
            GeoAPIVersion = Assembly.LoadFile(Path.Combine(appDir, "GeoAPI.dll")).GetName().Version.ToString();
        }

        public ModuleBuildInfoViewModel WorkbenchModuleBuildInfo { get; private set; }
        public ModuleBuildInfoViewModel ESMEModuleBuildInfo { get; private set; }
        public ModuleBuildInfoViewModel HRCModuleBuildInfo { get; private set; }
        public ModuleBuildInfoViewModel ViewsModuleBuildInfo { get; private set; }
        public string MapDllVersion { get; private set; }
        public string MapCoreVersion { get; private set; }
        public string NetTopologyVersion { get; private set; }
        public string GeoAPIVersion { get; private set; }

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
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Windows;
using ESME;
using ESME.Data;
using HRC.ViewModels;
using HRC.WPF;

namespace ESMEWorkbench.ViewModels.Main
{
    public partial class MainViewModel
    {
        #region Commands

        #region EditOptionsCommand
        public SimpleCommand<object, object> EditOptionsCommand
        {
            get
            {
                return _editOptions ?? (_editOptions = new SimpleCommand<object, object>(obj =>
                {
                    var programOptionsViewModel = new ApplicationOptionsViewModel(ESME.Globals.PluginManagerService);
                    var result = ESME.Globals.VisualizerService.ShowDialog("ApplicationOptionsView", programOptionsViewModel);
                    if ((result.HasValue) && (result.Value)) ESME.Globals.AppSettings.Save();
                    ESME.Globals.AppSettings = AppSettings.Load();
                    ESME.Globals.AppSettings = ESME.Globals.AppSettings;
                    //if (ESME.Globals.AppSettings != null && ESME.Globals.AppSettings.ScenarioDataDirectory != null &&
                    //    File.Exists(Path.Combine(ESME.Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv"))) 
                    //    InitializeEnvironmentManager();
                }));
            }
        }

        SimpleCommand<object, object> _editOptions;
        #endregion

        #region HelpCommand
        public SimpleCommand<object, object> HelpCommand
        {
            get { return _help ?? (_help = new SimpleCommand<object, object>(delegate { HelpHandler(); })); }
        }

        SimpleCommand<object, object> _help;

        void HelpHandler()
        {
            var userManual = Directory.GetFiles(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "ESME Workbench*Manual*.pdf");
            if (userManual.Length == 0)
            {
                ESME.Globals.MessageBoxService.ShowError("The user manual was not found!");
                return;
            }
            var info = new ProcessStartInfo(userManual[0]) { UseShellExecute = true, Verb = "open" };

            Process.Start(info);
        }
        #endregion

        #region DisabledCommand
        public SimpleCommand<object, object> DisabledCommand
        {
            get { return _disabled ?? (_disabled = new SimpleCommand<object, object>(arg => false, obj => { })); }
        }

        SimpleCommand<object, object> _disabled;
        #endregion

        #region ViewClosingCommand
        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand
        {
            get
            {
                return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(o =>
                { //todo
#if false 
                    if (SimulationProgressViewModel != null)
                    {
                        if (ESME.Globals.MessageBoxService.ShowOkCancel("A simulation is currently running.  OK to halt this simulation and exit ESME?", MessageBoxImage.Question) == MessageBoxResult.Cancel)
                        {
                            ((CancelEventArgs)o.EventArgs).Cancel = true;
                            return;
                        }
                        SimulationProgressViewModel.Simulation.Cancel();
                    } 
#endif
                    if (ESME.Globals.MasterDatabaseService.Context.IsModified) ESME.Globals.MasterDatabaseService.SaveChanges();
                    foreach (var popup in _openPopups.Where(popup => popup != null))
                        popup.Close();
                    
                    MediatorMessage.Send(MediatorMessage.ApplicationClosing, true);
                    ESME.Globals.AppSettings.Save();
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;
        #endregion
        
        #region AboutCommand
        public SimpleCommand<object, object> AboutCommand
        {
            get { return _about ?? (_about = new SimpleCommand<object, object>(arg => ShowAboutView())); }
        }

        SimpleCommand<object, object> _about;
        #endregion

        #region TestCommand
        public SimpleCommand<object, EventToCommandArgs> TestCommand { get { return _test ?? (_test = new SimpleCommand<object, EventToCommandArgs>(TestHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _test;

        void TestHandler(EventToCommandArgs args)
        {
            if (Scenario != null) ESME.Globals.OpenFileService.InitialDirectory = Scenario.StorageDirectoryPath;
            ESME.Globals.OpenFileService.Filter = "PGRID files (*.pgrid)|*.pgrid|All files (*.*)|*.*";
            var result = ESME.Globals.OpenFileService.ShowDialog((Window)ESME.Globals.ViewAwareStatusService.View);
            if (result.HasValue && result.Value)
            {
                //((RAMGeoEngine)ESME.Globals.PluginManagerService[PluginType.TransmissionLossCalculator][PluginSubtype.RAMGeo].DefaultPlugin).ReadRamPGrid(_openFile.FileName);
            }
        }
        #endregion

        #endregion
    }
}
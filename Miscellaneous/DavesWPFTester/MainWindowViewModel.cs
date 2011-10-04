using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.IO;
using System.Threading.Tasks;
using Cinch;
using ESME;
using ESME.Data;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Views.EnvironmentBuilder;
using HRC.Navigation;
using HRC.Services;
using MEFedMVVM.ViewModelLocator;
using Environment = System.Environment;

namespace DavesWPFTester
{
    [ExportViewModel("MainWindowViewModel")]
    public class MainWindowViewModel : ViewModelBase
    {
        readonly IMessageBoxService _messageBoxService;
        readonly IHRCOpenFileService _openFileService;
        readonly IHRCSaveFileService _saveFileService;
        readonly IViewAwareStatus _viewAwareStatus;
        readonly IUIVisualizerService _visualizerService;

        [ImportingConstructor]
        public MainWindowViewModel(IViewAwareStatus viewAwareStatus, IMessageBoxService messageBoxService, IHRCOpenFileService openFileService, IHRCSaveFileService saveFileService,
                                   IUIVisualizerService visualizerService)
        {
            System.Diagnostics.Process.GetCurrentProcess().PriorityClass = System.Diagnostics.ProcessPriorityClass.BelowNormal;
            _viewAwareStatus = viewAwareStatus;
            _messageBoxService = messageBoxService;
            _openFileService = openFileService;
            _saveFileService = saveFileService;
            _visualizerService = visualizerService;
            string settings = Path.Combine(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "One Navy Model"), "settings.xml");
            Globals.AppSettings = AppSettings.Load(settings);
            Initialize();
        }

        //public RangeComplexesViewModel RangeComplexesViewModel { get; private set; }
        public ImportProgressCollection ImportProgressCollection { get; private set; }
        public RangeComplexes RangeComplexes { get; set; }

        #region CreateManyRangeComplexesCommand
        SimpleCommand<object, object> _createManyTestRangeComplexes;

        public SimpleCommand<object, object> CreateManyRangeComplexesCommand
        {
            get
            {
                return _createManyTestRangeComplexes ??
                       (_createManyTestRangeComplexes =
                        new SimpleCommand<object, object>(delegate { CreateManyRangeComplexesHandler(); }));
            }
        }

        async void CreateManyRangeComplexesHandler()
        {
            var coordinates = new List<Geo>
                {
                    new Geo(29.3590, -79.2195),
                    new Geo(31.1627, -79.2195),
                    new Geo(31.1627, -81.2789),
                    new Geo(30.1627, -81.2789),
                    new Geo(29.3590, -80.8789),
                    new Geo(29.3590, -79.2195),
                };

            await TaskEx.Run(async () =>
            {
                for (var i = 0; i < 50; i++)
                {
                    try
                    {
                        while (!RangeComplexes.IsEnabled) await
                        TaskEx.Delay(10);
                        RangeComplexes.CreateRangeComplex(string.Format("Test {0:00}", i), 0, 29.3590, -79.2195, 0, coordinates, coordinates);
                    }
                    catch (Exception e)
                    {
                        _messageBoxService.ShowError(e.Message);
                    }
                }
            });
        }
        #endregion

        #region DeleteManyRangeComplexesCommand
        SimpleCommand<object, object> _deleteManyRangeComplexes;

        public SimpleCommand<object, object> DeleteManyRangeComplexesCommand
        {
            get
            {
                return _deleteManyRangeComplexes ??
                       (_deleteManyRangeComplexes =
                        new SimpleCommand<object, object>(delegate { DeleteManyRangeComplexesHandler(); }));
            }
        }

        async void DeleteManyRangeComplexesHandler()
        {
            await TaskEx.Run(async () =>
            {
                for (var i = 0; i < 50; i++)
                {
                    try
                    {
                        while (!RangeComplexes.IsEnabled) await
                        TaskEx.Delay(10);
                        RangeComplexes.RemoveRangeComplex(string.Format("Test {0:00}", i));
                    }
                    catch (ArgumentException) { }
                    catch (KeyNotFoundException) { }
                    catch (Exception e)
                    {
                        _messageBoxService.ShowError(e.Message);
                    }
                }
            });
        }
        #endregion

        #region CreateManyAreasCommand
        SimpleCommand<object, object> _createManyAreas;

        public SimpleCommand<object, object> CreateManyAreasCommand
        {
            get
            {
                return _createManyAreas ??
                       (_createManyAreas =
                        new SimpleCommand<object, object>(delegate { CreateManyAreasHandler(); }));
            }
        }

        async void CreateManyAreasHandler()
        {
            var coordinates = new List<Geo>
                {
                    new Geo(29.3590, -79.2195),
                    new Geo(31.1627, -79.2195),
                    new Geo(31.1627, -81.2789),
                    new Geo(30.1627, -81.2789),
                    new Geo(29.3590, -80.8789),
                    new Geo(29.3590, -79.2195),
                };

            for (var i = 0; i < 50; i++)
            {
                try
                {
                    var newAreaName = string.Format("Test {0:00}", i);
                    while (!RangeComplexes.IsEnabled) await TaskEx.Delay(10);
                    RangeComplexes[newAreaName].CreateArea(newAreaName, coordinates);
                } catch {}
            }
        }
        #endregion

        #region RemoveManyAreasCommand
        public SimpleCommand<object, object> RemoveManyAreasCommand
        {
            get { return _removeManyAreas ?? (_removeManyAreas = new SimpleCommand<object, object>(delegate { return IsRemoveManyAreasCommandEnabled; }, delegate { RemoveManyAreasHandler(); })); }
        }

        SimpleCommand<object, object> _removeManyAreas;

        bool IsRemoveManyAreasCommandEnabled
        {
            get { return true; }
        }

        async void RemoveManyAreasHandler()
        {
            for (var i = 0; i < 50; i++)
            {
                try
                {
                    var oldAreaName = string.Format("Test {0:00}", i);
                    while (!RangeComplexes.IsEnabled) await TaskEx.Delay(10);
                    RangeComplexes[oldAreaName].RemoveArea(oldAreaName);
                }
                catch { }
            }

        }
        #endregion

        #region CreateTestRangeComplexCommand
        SimpleCommand<object, object> _addDave;

        public SimpleCommand<object, object> CreateTestRangeComplexCommand
        {
            get { return _addDave ?? (_addDave = new SimpleCommand<object, object>(delegate { CreateTestRangeComplexHandler(); })); }
        }

        async void CreateTestRangeComplexHandler()
        {
            var coordinates = new List<Geo>
            {
                new Geo(29.3590, -79.2195),
                new Geo(31.1627, -79.2195),
                new Geo(31.1627, -81.2789),
                new Geo(30.1627, -81.2789),
                new Geo(29.3590, -80.8789),
                new Geo(29.3590, -79.2195),
            };
            try
            {
                while (!RangeComplexes.IsEnabled) await TaskEx.Delay(10);
                RangeComplexes.CreateRangeComplex("Test", 0, 29.3590, -79.2195, 0, coordinates, coordinates);
            }
            catch (Exception e)
            {
                _messageBoxService.ShowError(e.Message);
            }
        }
        #endregion

        #region RemoveTestRangeComplexCommand
        SimpleCommand<object, object> _removeTestRangeComplex;

        public SimpleCommand<object, object> RemoveTestRangeComplexCommand
        {
            get { return _removeTestRangeComplex ?? (_removeTestRangeComplex = new SimpleCommand<object, object>(delegate { RemoveTestRangeComplexHandler(); })); }
        }

        async void RemoveTestRangeComplexHandler()
        {
            try
            {
                while (!RangeComplexes.IsEnabled) await TaskEx.Delay(10);
                RangeComplexes.RemoveRangeComplex("Test");
            }
            catch (KeyNotFoundException) { }
            catch (Exception e)
            {
                _messageBoxService.ShowError(e.Message);
            }
        }
        #endregion

        #region CreateTestAreaCommand
        SimpleCommand<object, object> _addTestArea;

        public SimpleCommand<object, object> CreateTestAreaCommand
        {
            get { return _addTestArea ?? (_addTestArea = new SimpleCommand<object, object>(delegate { AddTestAreaHandler(); })); }
        }

        async void AddTestAreaHandler()
        {
            var coordinates = new List<Geo>
            {
                new Geo(29.3590, -79.2195),
                new Geo(31.1627, -79.2195),
                new Geo(31.1627, -81.2789),
                new Geo(30.1627, -81.2789),
                new Geo(29.3590, -80.8789),
                new Geo(29.3590, -79.2195),
            };
            try
            {
                while (RangeComplexes.RangeComplexCollection["Test"].IsLoading) await TaskEx.Delay(10);
                RangeComplexes.RangeComplexCollection["Test"].CreateArea("Test", coordinates);
            }
            catch (Exception e)
            {
                _messageBoxService.ShowError(e.Message);
            }
        }
        #endregion

        #region RemoveTestAreaCommand
        SimpleCommand<object, object> _removeTestArea;

        public SimpleCommand<object, object> RemoveTestAreaCommand
        {
            get { return _removeTestArea ?? (_removeTestArea = new SimpleCommand<object, object>(delegate { RemoveTestAreaHandler(); })); }
        }

        async void RemoveTestAreaHandler()
        {
            try
            {
                while (RangeComplexes.RangeComplexCollection["Test"].IsLoading) await TaskEx.Delay(10);
                RangeComplexes.RangeComplexCollection["Test"].RemoveArea("Test");
            }
            catch (Exception e)
            {
                _messageBoxService.ShowError(e.Message);
            }
        }
        #endregion

        #region DumpTestAreaCommand
        SimpleCommand<object, object> _dumpTestArea;

        public SimpleCommand<object, object> DumpTestAreaCommand
        {
            get { return _dumpTestArea ?? (_dumpTestArea = new SimpleCommand<object, object>(delegate { DumpTestAreaHandler(); })); }
        }

        void DumpTestAreaHandler()
        {
            try
            {
                RangeComplexes.Dump();
            }
            catch (Exception e)
            {
                _messageBoxService.ShowError(e.Message);
            }
        }
        #endregion

        #region ImportTestBathymetryCommand
        public SimpleCommand<object, object> ImportTestBathymetryCommand
        {
            get { return _importTestBathymetry ?? (_importTestBathymetry = new SimpleCommand<object, object>(delegate { return IsImportTestBathymetryCommandEnabled; }, delegate { ImportTestBathymetryHandler(); })); }
        }

        SimpleCommand<object, object> _importTestBathymetry;

        bool IsImportTestBathymetryCommandEnabled
        {
            get { return true; }
        }

        void ImportTestBathymetryHandler()
        {
            try
            {
                RangeComplexes["Jacksonville"].AreaCollection["Jax_Ops_Area"].ImportBathymetry(RangeComplexes["Jacksonville"].AreaCollection["Jax_Ops_Area"]["0.50min.bathymetry"]);
            }
            catch (Exception e)
            {
                _messageBoxService.ShowError(e.Message);
            }
        }
        #endregion

        #region RemoveTestBathymetryCommand
        public SimpleCommand<object, object> RemoveTestBathymetryCommand
        {
            get { return _removeTestBathymetry ?? (_removeTestBathymetry = new SimpleCommand<object, object>(delegate { return IsRemoveTestBathymetryCommandEnabled; }, delegate { RemoveTestBathymetryHandler(); })); }
        }

        SimpleCommand<object, object> _removeTestBathymetry;

        bool IsRemoveTestBathymetryCommandEnabled
        {
            get { return true; }
        }

        void RemoveTestBathymetryHandler()
        {
            try
            {
                RangeComplexes["Jacksonville"].AreaCollection["Jax_Ops_Area"].RemoveBathymetry(RangeComplexes["Jacksonville"].AreaCollection["Jax_Ops_Area"]["0.50min.bathymetry"]);
            }
            catch (Exception e)
            {
                _messageBoxService.ShowError(e.Message);
            }
        }
        #endregion

        #region LoadTestBathymetryCommand
        public SimpleCommand<object, object> LoadTestBathymetryCommand
        {
            get { return _loadTestBathymetry ?? (_loadTestBathymetry = new SimpleCommand<object, object>(delegate { return IsLoadTestBathymetryCommandEnabled; }, delegate { LoadTestBathymetryHandler(); })); }
        }

        SimpleCommand<object, object> _loadTestBathymetry;

        bool IsLoadTestBathymetryCommandEnabled
        {
            get { return true; }
        }

        void LoadTestBathymetryHandler()
        {
            try
            {
                RangeComplexes["Jacksonville"].AreaCollection["Jax_Ops_Area"]["0.50min.bathymetry"].GetMyDataAsync();
            }
            catch (Exception e)
            {
                _messageBoxService.ShowError(e.Message);
            }
        }
        #endregion

        #region ClearTestBathymetryCommand
        public SimpleCommand<object, object> ClearTestBathymetryCommand
        {
            get { return _clearTestBathymetry ?? (_clearTestBathymetry = new SimpleCommand<object, object>(delegate { return IsClearTestBathymetryCommandEnabled; }, delegate { ClearTestBathymetryHandler(); })); }
        }

        SimpleCommand<object, object> _clearTestBathymetry;

        bool IsClearTestBathymetryCommandEnabled
        {
            get { return true; }
        }

        void ClearTestBathymetryHandler()
        {
            RangeComplexes["Jacksonville"].AreaCollection["Jax_Ops_Area"]["0.50min.bathymetry"].Reset();
        }
        #endregion

        #region LoadEnvironmentCommand
        public SimpleCommand<object, object> LoadEnvironmentCommand
        {
            get { return _loadEnvironment ?? (_loadEnvironment = new SimpleCommand<object, object>(delegate { return IsLoadEnvironmentCommandEnabled; }, delegate { LoadEnvironmentHandler(); })); }
        }

        SimpleCommand<object, object> _loadEnvironment;

        bool IsLoadEnvironmentCommandEnabled
        {
            get { return true; }
        }

        void LoadEnvironmentHandler()
        {
            if (RangeComplexes.SelectedBathymetry != BathymetryFile.None) RangeComplexes.SelectedBathymetry.GetMyDataAsync();
            if (RangeComplexes.SelectedWind != WindFile.None) RangeComplexes.SelectedWind.GetMyDataAsync();
            if (RangeComplexes.SelectedBottomLoss != BottomLossFile.None) RangeComplexes.SelectedBottomLoss.GetMyDataAsync();
            if (RangeComplexes.SelectedSediment != SedimentFile.None) RangeComplexes.SelectedSediment.GetMyDataAsync();
            if (RangeComplexes.SelectedSoundSpeed != SoundSpeedFile.None) RangeComplexes.SelectedSoundSpeed.GetMyDataAsync();
        }
        #endregion

        #region ClearEnvironmentCommand
        public SimpleCommand<object, object> ClearEnvironmentCommand
        {
            get { return _clearEnvironment ?? (_clearEnvironment = new SimpleCommand<object, object>(delegate { return IsClearEnvironmentCommandEnabled; }, delegate { ClearEnvironmentHandler(); })); }
        }

        SimpleCommand<object, object> _clearEnvironment;

        bool IsClearEnvironmentCommandEnabled
        {
            get { return true; }
        }

        void ClearEnvironmentHandler()
        {
            if (RangeComplexes.SelectedBathymetry != BathymetryFile.None) RangeComplexes.SelectedBathymetry.Reset();
            if (RangeComplexes.SelectedWind != WindFile.None) RangeComplexes.SelectedWind.Reset();
            if (RangeComplexes.SelectedBottomLoss != BottomLossFile.None) RangeComplexes.SelectedBottomLoss.Reset();
            if (RangeComplexes.SelectedSediment != SedimentFile.None) RangeComplexes.SelectedSediment.Reset();
            if (RangeComplexes.SelectedSoundSpeed != SoundSpeedFile.None) RangeComplexes.SelectedSoundSpeed.Reset();
        }
        #endregion
        async void Initialize()
        {
            RangeComplexes = RangeComplexes.Singleton;
            //RangeComplexesViewModel = new RangeComplexesViewModel(RangeComplexes.Singleton);
            ImportProgressCollection = ImportProgressCollection.Singleton;
            try
            {
                await RangeComplexes.ReadRangeComplexFileAsync(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv"));
            }
            catch (Exception e)
            {
                _messageBoxService.ShowError(e.Message);
            }
        }
    }
}
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
            _viewAwareStatus = viewAwareStatus;
            _messageBoxService = messageBoxService;
            _openFileService = openFileService;
            _saveFileService = saveFileService;
            _visualizerService = visualizerService;
            string settings = Path.Combine(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME WorkBench"), "settings.xml");
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
                        RangeComplexes.CreateRangeComplex(string.Format("Test{0}", i), 0, 29.3590, -79.2195, 0, coordinates, coordinates);
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
                        RangeComplexes.RemoveRangeComplex(string.Format("Test{0}", i));
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
                    while (!RangeComplexes.IsEnabled) await TaskEx.Delay(10);
                    RangeComplexes.RangeComplexCollection[string.Format("Test{0}", i)].CreateArea(string.Format("Test{0}", i), coordinates);
                } catch {}
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
                while (!RangeComplexes.RangeComplexCollection["Test"].IsLoading) await TaskEx.Delay(10);
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
                while (!RangeComplexes.RangeComplexCollection["Test"].IsLoading) await TaskEx.Delay(10);
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

        void LoadTestBathymetryHandler() { RangeComplexes["Jacksonville"].AreaCollection["Jax_Ops_Area_200km"]["0.50min.bathymetry"].GetMyDataAsync(); }
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
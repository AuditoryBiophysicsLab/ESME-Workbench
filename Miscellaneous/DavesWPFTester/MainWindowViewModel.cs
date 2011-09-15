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

        public RangeComplexesViewModel RangeComplexesViewModel { get; private set; }
        public ImportProgressCollection ImportProgressCollection { get; private set; }
        public RangeComplexes RangeComplexes { get; set; }

        #region ThrashTestRangeComplexesCommand
        SimpleCommand<object, object> _thrashTestRangeComplexes;

        public SimpleCommand<object, object> ThrashTestRangeComplexesCommand
        {
            get
            {
                return _thrashTestRangeComplexes ??
                       (_thrashTestRangeComplexes =
                        new SimpleCommand<object, object>(delegate { return true; },
                                                          delegate { ThrashTestRangeComplexesHandler(); }));
            }
        }

        async void ThrashTestRangeComplexesHandler()
        {
            for (int i = 0; i < 50; i++)
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
                    RangeComplexes.Singleton.CreateRangeComplex(string.Format("Test{0}", i), 0, 29.3590, -79.2195, 0,
                                                                coordinates, coordinates);
                }
                catch (Exception e)
                {
                    _messageBoxService.ShowError(e.Message);
                }
                await TaskEx.Delay(500);
            }
        }
        #endregion

        #region ThrashTestAreasCommand
        SimpleCommand<object, object> _thrashTestAreas;

        public SimpleCommand<object, object> ThrashTestAreasCommand
        {
            get
            {
                return _thrashTestAreas ??
                       (_thrashTestAreas =
                        new SimpleCommand<object, object>(delegate { return IsThrashTestAreasCommandEnabled; },
                                                          delegate { ThrashTestAreasHandler(); }));
            }
        }

        bool IsThrashTestAreasCommandEnabled
        {
            get { return true; }
        }

        async 

        void ThrashTestAreasHandler()
        {
            for (int i = 0; i < 50; i++)
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
                    RangeComplexes.Singleton.RangeComplexCollection[string.Format("Test{0}", i)].CreateArea(
                                                                                                            string.
                                                                                                                Format
                                                                                                                ("Test{0}",
                                                                                                                 i),
                                                                                                            coordinates);
                }
                catch (Exception e)
                {
                    _messageBoxService.ShowError(e.Message);
                }
                await
                TaskEx.Delay(500);
            }
        }
        #endregion

        #region DeleteTestRangeComplexesCommand
        SimpleCommand<object, object> _deleteTestRangeComplexes;

        public SimpleCommand<object, object> DeleteTestRangeComplexesCommand
        {
            get
            {
                return _deleteTestRangeComplexes ??
                       (_deleteTestRangeComplexes =
                        new SimpleCommand<object, object>(delegate { return IsDeleteTestRangeComplexesCommandEnabled; },
                                                          delegate { DeleteTestRangeComplexesHandler(); }));
            }
        }

        bool IsDeleteTestRangeComplexesCommandEnabled
        {
            get { return true; }
        }

        async 

        void DeleteTestRangeComplexesHandler()
        {
            for (int i = 0; i < 50; i++)
            {
                try
                {
                    RangeComplexes.Singleton.RemoveRangeComplex(string.Format("Test{0}", i));
                }
                catch (ArgumentException) {}
                catch (Exception e)
                {
                    _messageBoxService.ShowError(e.Message);
                }

                await
                TaskEx.Delay(100);
            }
        }
        #endregion

        #region CreateTestRangeComplexCommand
        SimpleCommand<object, object> _addDave;

        public SimpleCommand<object, object> CreateTestRangeComplexCommand
        {
            get { return _addDave ?? (_addDave = new SimpleCommand<object, object>(delegate { CreateTestRangeComplexHandler(); })); }
        }

        void CreateTestRangeComplexHandler()
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
                RangeComplexes.Singleton.CreateRangeComplex("Test", 0, 29.3590, -79.2195, 0, coordinates, coordinates);
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

        void RemoveTestRangeComplexHandler()
        {
            try
            {
                RangeComplexes.Singleton.RemoveRangeComplex("Test");
            }
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

        void AddTestAreaHandler()
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
                RangeComplexes.Singleton.RangeComplexCollection["Test"].CreateArea("Test", coordinates);
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

        void RemoveTestAreaHandler()
        {
            try
            {
                RangeComplexes.Singleton.RangeComplexCollection["Test"].RemoveArea("Test");
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
                RangeComplexes.Singleton.Dump();
            }
            catch (Exception e)
            {
                _messageBoxService.ShowError(e.Message);
            }
        }
        #endregion

        async 

        void Initialize()
        {
            RangeComplexes = RangeComplexes.Singleton;
            RangeComplexesViewModel = new RangeComplexesViewModel(RangeComplexes.Singleton);
            ImportProgressCollection = ImportProgressCollection.Singleton;
            try
            {
                await
                RangeComplexes.Singleton.ReadRangeComplexFileAsync(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv"));
            }
            catch (Exception e)
            {
                _messageBoxService.ShowError(e.Message);
            }
        }
    }
}
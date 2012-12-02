using System;
using System.Collections.ObjectModel;
using System.Data.Entity;
using System.Diagnostics;
using System.Linq;
using System.Windows;
using ESME.PSM;
using ESME.Scenarios;
using ESME.Views.Scenarios;
using HRC;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.PSM
{
    /// <summary>
    ///   To create and show the view as a dialog: var vm = new PSMTreeViewModel {...}; var result = _visualizerService.ShowDialog("PSMTreeView", vm); if ((!result.HasValue) || (!result.Value)) return; To create and show the view as a window: var vm = new PSMTreeViewModel {...}; var window = _visualizerService.ShowWindow("PSMTreeView", vm);
    /// </summary>
    public class PSMTreeViewModel : ViewModelBase
    {
        readonly PSMContext _context;
        public ObservableCollection<Platform> Platforms { get; set; }
        public ObservableCollection<Source> Sources { get; set; }
        public ObservableCollection<Mode> Modes { get; set; }
        public object DisplayedView { get; set; }
        public object CopiedObject { get; set; }
        public PSMTreeViewModel(string psmDatabasePath)
        {
            #region Mediator Message registration
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nPSMTreeViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
            #endregion

            _context = PSMContext.Create(psmDatabasePath);
            var modes = (from mode in _context.Modes
                         orderby mode.ModeName
                         select mode);
            Modes = _context.Modes.Local;
            var sources = (from source in _context.Sources
                               .Include(s => s.Modes)
                           orderby source.SourceName
                           select source);
            Sources = _context.Sources.Local;
            var platforms = (from platform in _context.Platforms
                                 .Include(p => p.Sources)
                             orderby platform.PlatformName
                             select platform);
            Platforms = _context.Platforms.Local;
        }

        public PSMTreeViewModel() { }

        #region platforms

        public void AddPlatform(Platform platform)
        {
            _context.Platforms.Add(platform);
            Platforms.Add(platform);
            _context.SaveChanges();
        }

        #region NewPlatformCommand
        public SimpleCommand<object, EventToCommandArgs> NewPlatformCommand
        {
            get { return _newPlatform ?? (_newPlatform = new SimpleCommand<object, EventToCommandArgs>(NewPlatformHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _newPlatform;

        void NewPlatformHandler(EventToCommandArgs args)
        {
            var platform = Platform.NewPSMPlatform();
            var vm = new PropertiesViewModel
            {
                PropertyObject = platform,
                IsPSMView = true,
            };
            DisplayedView = new PlatformPropertiesControlView { DataContext = vm };
        }
        #endregion

        [MediatorMessageSink(MediatorMessage.CopyPSMPlatform), UsedImplicitly]
        void CopyPlatform(Platform platform)
        {
            var newplatform = new Platform(platform);
            newplatform.PlatformName = newplatform.PlatformName + " (copy)";
            AddPlatform(newplatform);
        }

        [MediatorMessageSink(MediatorMessage.EditPSMPlatform), UsedImplicitly]
        void EditPlatform(Platform platform)
        {
            var vm = new PropertiesViewModel
            {
                PropertyObject = platform,
                IsPSMView = true,
            };
            DisplayedView = new PlatformPropertiesControlView { DataContext = vm };
        }

        [MediatorMessageSink(MediatorMessage.PSMPlatformChanged), UsedImplicitly]
        void UpdatePlatforms(Platform platform)
        {
            DisplayedView = null;
        }

        [MediatorMessageSink(MediatorMessage.DeletePSMPlatform), UsedImplicitly]
        void DeletePlatform(Platform platform)
        {
            Platforms.Remove(platform);
            foreach (var source in platform.Sources) DeleteSource(source);
            _context.Platforms.Remove(platform);
            _context.SaveChanges();
        }

        [MediatorMessageSink(MediatorMessage.PastePSMSource),UsedImplicitly]
        void PasteSource(Platform platform)
        {
            if (!(CopiedObject is Source)) return;
            var source = (Source)CopiedObject;
            source.Platform = platform;
        }

        #endregion

        #region sources
        public void AddSourceToContext(Source source)
        {
            _context.Sources.Add(source);
            Sources.Add(source);
        }

        [MediatorMessageSink(MediatorMessage.AddPSMSource), UsedImplicitly]
        void AddSource(Source source)
        {
            AddSourceToContext(source);
            var vm = new PropertiesViewModel
            {
                PropertyObject = source,
                IsPSMView = true,
            };
            DisplayedView = new SourcePropertiesControlView { DataContext = vm };
        }

        [MediatorMessageSink(MediatorMessage.CopyPSMSource), UsedImplicitly]
        void CopySource(Source source) { CopiedObject = source; }

        [MediatorMessageSink(MediatorMessage.EditPSMSource), UsedImplicitly]
        void EditSource(Source source)
        {
            var vm = new PropertiesViewModel
            {
                PropertyObject = source,
                IsPSMView = true,
            };
            DisplayedView = new SourcePropertiesControlView { DataContext = vm };
        }

        [MediatorMessageSink(MediatorMessage.PSMSourceChanged), UsedImplicitly]
        void UpdateSource(Source source) { DisplayedView = null; }

        [MediatorMessageSink(MediatorMessage.DeletePSMSource), UsedImplicitly]
        void DeleteSource(Source source)
        {
            Sources.Remove(source);
            foreach (var mode in source.Modes) DeleteMode(mode);
            _context.Sources.Remove(source);
            _context.SaveChanges();
        }
        #endregion

        #region modes

        public void AddModeToContext(Mode mode)
        {
            _context.Modes.Add(mode);
            Modes.Add(mode);
        }

        [MediatorMessageSink(MediatorMessage.AddPSMMode), UsedImplicitly]
        void AddMode(Mode mode)
        {
            AddModeToContext(mode);
            var vm = new ModePropertiesViewModel(mode) { IsPSMView = true, };
            DisplayedView = new ModePropertiesControlView { DataContext = vm };
        }

        [MediatorMessageSink(MediatorMessage.CopyPSMMode), UsedImplicitly]
        void CopyMode(Mode mode)
        {
            var newmode = new Mode(mode);
        }

        [MediatorMessageSink(MediatorMessage.EditPSMMode), UsedImplicitly]
        void EditMode(Mode mode)
        {
            var vm = new ModePropertiesViewModel(mode) { IsPSMView = true, };
            DisplayedView = new ModePropertiesControlView { DataContext = vm, };
        }

        [MediatorMessageSink(MediatorMessage.PSMModeChanged), UsedImplicitly]
        void UpdateMode(Mode mode) { DisplayedView = null; }

        [MediatorMessageSink(MediatorMessage.DeletePSMMode), UsedImplicitly]
        void DeleteMode(Mode mode)
        {
            Modes.Remove(mode);
            _context.Modes.Remove(mode);
            _context.SaveChanges();
        }

        #endregion

        #region ViewLoadedCommand
        public SimpleCommand<object, EventToCommandArgs> ViewLoadedCommand
        {
            get { return _viewLoaded ?? (_viewLoaded = new SimpleCommand<object, EventToCommandArgs>(ViewLoadedHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _viewLoaded;
        PSMTreeView _view;
        void ViewLoadedHandler(EventToCommandArgs args)
        {
            var routedEventArgs = (RoutedEventArgs)args.EventArgs;
            var source = routedEventArgs.Source;
            _view = (PSMTreeView)source;
        }
        #endregion

        #region ViewClosingCommand
        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand
        {
            get { return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(ViewClosingHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;

        void ViewClosingHandler(EventToCommandArgs args)
        {
            try
            {
                _context.SaveChanges();
            }
            catch (Exception e)
            {
                throw new ApplicationException("caught error in saveChanges, " + e.Message);
            }
        }
        #endregion

        #region Design-time data
        public static PSMTreeViewModel DesignTimeData { get; set; }

        static PSMTreeViewModel()
        {
#if true
            DesignTimeData = new PSMTreeViewModel
            {
                Platforms = new ObservableCollection<Platform>
                {
                    new Platform
                    {
                        PlatformName = "Platform1",
                        Sources = new ObservableList<Source>
                        {
                            new Source
                            {
                                SourceName = "Source 1",
                                Modes = new ObservableList<Mode>
                                {
                                    new Mode
                                    {
                                        ModeName = "Mode 1",
                                    },
                                    new Mode
                                    {
                                        ModeName = "Mode 2",
                                    },
                                },
                            },
                            new Source
                            {
                                SourceName = "Source 2"
                            },
                        },
                    },
                    new Platform
                    {
                        PlatformName = "Platform2",
                    }
                },
            };

#endif
        }
        #endregion

        public void SeedTestValues()
        {
            var m1 = Mode.NewPSMMode();
            m1.ModeName = "Mode 1";

            var m2 = Mode.NewPSMMode();
            m2.ModeName = "mode 2";

            var s1 = Source.NewPSMSource();
            s1.SourceName = "Source 1";
            s1.SourceType = "demo source";
            s1.Modes = new ObservableList<Mode> { m1 };

            var s2 = Source.NewPSMSource();
            s2.SourceName = "Source 2";
            s2.SourceType = "demo source";
            s2.Modes = new ObservableList<Mode> { m2 };

            var p1 = Platform.NewPSMPlatform();
            p1.PlatformName = "Platform 1";
            p1.PlatformType = "a test platform";
            p1.Sources = new ObservableList<Source> { s1 };

            var p2 = Platform.NewPSMPlatform();
            p2.PlatformName = "Platform 2";
            p2.PlatformType = "a test platform";
            p2.Sources = new ObservableList<Source> { s2 };

            var p3 = Platform.NewPSMPlatform();
            p3.PlatformName = "Platform 3";
            p3.PlatformType = "a test platform";

            _context.Modes.Add(m1);
            _context.Modes.Add(m2);

            _context.Sources.Add(s1);
            _context.Sources.Add(s2);

            _context.Platforms.Add(p1);
            _context.Platforms.Add(p2);
            _context.Platforms.Add(p3);

            _context.SaveChanges();
        }
    }
}
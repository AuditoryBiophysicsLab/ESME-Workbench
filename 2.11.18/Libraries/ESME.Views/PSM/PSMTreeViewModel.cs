using System;
using System.Collections.ObjectModel;
using System.Data.Entity;
using System.Diagnostics;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
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
    /// To create and show the view as a dialog:
    /// var vm = new PSMTreeViewModel {...};
    /// var result = _visualizerService.ShowDialog("PSMTreeView", vm);
    /// if ((!result.HasValue) || (!result.Value)) return;
    /// 
    /// To create and show the view as a window:
    /// var vm = new PSMTreeViewModel {...};
    /// var window = _visualizerService.ShowWindow("PSMTreeView", vm);
    /// </summary>
    public class PSMTreeViewModel : ViewModelBase
    {
        readonly PSMContext _context;
        public ObservableCollection<Platform> Platforms { get; set; }
        public ObservableCollection<Source> Sources { get; set; }
        public ObservableCollection<Mode> Modes { get; set; }
        public object DisplayedView { get; set; }

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

        public PSMTreeViewModel() {
            
        }
        

        public void AddPlatformToContext(Platform platform)
        {
            _context.Platforms.Add(platform);
            Platforms.Add(platform);
        }

        #region NewPlatformCommand
        public SimpleCommand<object, EventToCommandArgs> NewPlatformCommand
        {
            get { return _newPlatform ?? (_newPlatform = new SimpleCommand<object, EventToCommandArgs>(NewPlatformHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _newPlatform;

        void NewPlatformHandler(EventToCommandArgs args)
        {
            var platform = new Platform(){PlatformName = "New Platform",IsNew = true,};
            Platforms.Add(platform);
            var vm = new PropertiesViewModel()
            {
                PropertyObject = platform,
                IsPSMView = true,
            };
            DisplayedView = new PlatformPropertiesControlView() { DataContext = vm };
        }
        #endregion

        [MediatorMessageSink(MediatorMessage.PSMPlatformChanged), UsedImplicitly]
        void UpdatePlatforms(Platform platform)
        {
            AddPlatformToContext(platform);
            //_context.SaveChanges();
        }

        [MediatorMessageSink(MediatorMessage.EditPSMPlatform), UsedImplicitly]
        void EditPlatform(Platform platform)
        {
            var vm = new PropertiesViewModel()
            {
                PropertyObject = platform,
                IsPSMView = true,
            };
            DisplayedView = new PlatformPropertiesControlView() { DataContext = vm };
        }

        public void AddSourceToContext(Source source)
        {
            _context.Sources.Add(source);
            Sources.Add(source);
        }

        [MediatorMessageSink(MediatorMessage.AddPSMSource), UsedImplicitly]
        void AddSource(Source source)
        {
            //AddSourceToContext(source);
            //Sources.Add(source);
            var vm = new PropertiesViewModel()
            {
                PropertyObject = source,
                IsPSMView = true,
            };
            DisplayedView = new SourcePropertiesControlView() { DataContext = vm };
        }

        [MediatorMessageSink(MediatorMessage.EditPSMSource), UsedImplicitly]
        void EditSource(Source source)
        {
            var vm = new PropertiesViewModel()
            {
                PropertyObject = source,
                IsPSMView = true,
            };
            DisplayedView = new SourcePropertiesControlView() { DataContext = vm };
        }

        public void AddModeToContext(Mode mode)
        {
            _context.Modes.Add(mode);
            Modes.Add(mode);
        }

        [MediatorMessageSink(MediatorMessage.AddPSMMode), UsedImplicitly]
        void AddMode(Mode mode)
        {
         //   AddModeToContext(mode);
            Modes.Add(mode);
            var vm = new ModePropertiesViewModel(mode) { IsPSMView = true, };
            DisplayedView = new ModePropertiesControlView { DataContext = vm };
        }

        [MediatorMessageSink(MediatorMessage.EditPSMMode), UsedImplicitly]
        void EditMode(Mode mode)
        {
            var vm = new ModePropertiesViewModel(mode) { IsPSMView = true, };
            DisplayedView = new ModePropertiesControlView { DataContext = vm, };
        }

        [MediatorMessageSink(MediatorMessage.PSMModeChanged),UsedImplicitly]
        void ModeChanged(Mode mode)
        {
            //replace it in the tree view list

            //update the database context 

            //propagate changes to all other sources that point to this mode -- or are we instead creating a new mode?

            //save changes?
        }

        #region ViewLoadedCommand
        PSMTreeView _view;
        public SimpleCommand<object, EventToCommandArgs> ViewLoadedCommand
        {
            get { return _viewLoaded ?? (_viewLoaded = new SimpleCommand<object, EventToCommandArgs>(ViewLoadedHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _viewLoaded;

        void ViewLoadedHandler(EventToCommandArgs args)
        {
            var routedEventArgs = (RoutedEventArgs)args.EventArgs;
            var source = routedEventArgs.Source;
            _view = (PSMTreeView)source;
        }
        #endregion

        #region Design-time data
        public static PSMTreeViewModel DesignTimeData { get; set; }
        static PSMTreeViewModel()
        {

#if true
            DesignTimeData = new PSMTreeViewModel()
                   {
                       Platforms = new ObservableCollection<Platform>()
                {
                    new Platform()
                    {
                        PlatformName = "Platform1",
                        Sources = new ObservableList<Source>()
                        {
                            new Source()
                            {
                                SourceName = "Source 1",
                                Modes = new ObservableList<Mode>()
                                {
                                    new Mode()
                                    {
                                        ModeName = "Mode 1",
                                    },
                                    new Mode()
                                    {
                                        ModeName = "Mode 2",
                                    },
                                },
                            },
                            new Source()
                            {
                                SourceName = "Source 2"
                            },
                        },
                    },
                    new Platform()
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
            var m1 = new Mode()
            {
                ModeName = "Mode 1",
            };
            _context.Modes.Add(m1);
            var m2 = new Mode()
            {
                ModeName = "Mode 2",
            };
            _context.Modes.Add(m2);
            var s1 = new Source()
            {
                SourceName = "Source 1",
                SourceType = "demo source",
                Modes = new ObservableList<Mode>()
                {
                    m1,
                }
            };
            _context.Sources.Add(s1);
            var s2 = new Source()
            {
                SourceName = "Source 2",
                SourceType = "demo source",
                Modes = new ObservableList<Mode>()
                {
                    m2,
                }
            };
            _context.Sources.Add(s2);


            var p1 = new Platform()
            {
                PlatformName = "Platform 1",
                PlatformType = "A test platform",
                Sources = new ObservableList<Source>()
                { s1,}
            };

            var p2 = new Platform()
            {
                PlatformName = "Platform 2",
                PlatformType = "A test platform",
                Sources = new ObservableList<Source>() {s2}
            };

            var p3 = new Platform()
            {
                PlatformName = "Platform 3",
                PlatformType = "A test platform",
                Sources = new ObservableList<Source>() { s2,s1 }
            };
            
            
            _context.Platforms.Add(p1);
            _context.Platforms.Add(p2);
           // _context.Platforms.Add(p3);


        }
    }
}

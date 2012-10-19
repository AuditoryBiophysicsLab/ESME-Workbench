using System;
using System.Collections.ObjectModel;
using System.Data.Entity;
using System.Linq;
using System.Windows;
using ESME.PSM;
using ESME.Scenarios;
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
        public void AddPlatform(Platform platform) { _context.Platforms.Add(platform); }
        public void AddSource(Source source) { _context.Sources.Add(source); }
        public void AddMode(Mode mode) { _context.Modes.Add(mode); }

        public PSMTreeViewModel(string psmDatabasePath)
        {
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

        #region Design-time data
        public static PSMTreeViewModel DesignTimeData { get; set; }
        static PSMTreeViewModel()
        {
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
        }
        #endregion

        #region NewCommand
        public SimpleCommand<object, EventToCommandArgs> NewCommand
        {
            get { return _new ?? (_new = new SimpleCommand<object, EventToCommandArgs>(NewHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _new;

        void NewHandler(EventToCommandArgs args)
        {
            var routedEventArgs = (RoutedEventArgs)args.EventArgs;
            var source = routedEventArgs.Source;
            switch (source.ToString())
            {
                case "Platform":
                    NewPlatformHandler();
                    break;
                case "Source":
                    NewSourceHandler();
                    break;
                case "Mode":
                    NewModeHandler();
                    break;
                default:
                    throw new ArgumentException("selected item type unrecognized");
            }
        }

        void NewModeHandler() { }

        void NewSourceHandler() { }

        void NewPlatformHandler() { }
        #endregion

        #region EditCommand
        public SimpleCommand<object, EventToCommandArgs> EditCommand
        {
            get { return _edit ?? (_edit = new SimpleCommand<object, EventToCommandArgs>(EditHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _edit;

        static void EditHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter;
        }
        #endregion

        #region DeleteCommand
        public SimpleCommand<object, EventToCommandArgs> DeleteCommand
        {
            get { return _delete ?? (_delete = new SimpleCommand<object, EventToCommandArgs>(DeleteHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _delete;

        static void DeleteHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter;
        }
        #endregion

        #region OkCommand
        public SimpleCommand<object, EventToCommandArgs> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, EventToCommandArgs>(OkHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _ok;

        static void OkHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter;
        }
        #endregion

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
    }
}

using System;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Diagnostics;
using System.Reflection;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using HRC.Utility;

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
            Initialize();
        }

        void Initialize()
        {
            Column1Items = new ObservableCollection<LabelValuePair>
                           {
                               new LabelValuePair
                               {
                                   Label = "Product",
                                   Value = _assemblyTitle,
                               },
                               new LabelValuePair
                               {
                                   Label = "Description",
                                   Value = _assemblyDescription,
                               },
                               new LabelValuePair
                               {
                                   Label = "Version",
                                   Value = _assemblyName.Version.ToString(),
                               },
                               new LabelValuePair
                               {
                                   Label = "Build date/time",
                                   Value = AssemblyTricks.RetrieveLinkerTimestamp().ToString(),
                               },
                           };
            Column2Items = new ObservableCollection<LabelValuePair>
                           {
                               new LabelValuePair
                               {
                                   Label = "Architect and Lead Developer",
                                   Value = "Dave Anderson"
                               },
                               new LabelValuePair
                               {
                                   Label = "Development team",
                                   Value = "Graham Voysey"
                               },
                               new LabelValuePair
                               {
                                   Label = "",
                                   Value = "Andrew Brughera"
                               },
                               new LabelValuePair
                               {
                                   Label = "Testing",
                                   Value = "Lenley Woodard"
                               },
                               new LabelValuePair
                               {
                                   Label = "",
                                   Value = "Scott Schecklman"
                               },
                               new LabelValuePair
                               {
                                   Label = "Marine Mammal Simulator",
                                   Value = "Dorian Houser"
                               },
                               new LabelValuePair
                               {
                                   Label = "",
                                   Value = "Matt Cross"
                               },
                           };
            NotifyPropertyChanged(Column1ItemsChangedEventArgs);
            NotifyPropertyChanged(Column2ItemsChangedEventArgs);
        }

        #region public ObservableCollection<LabelValuePair> Column1Items { get; set; }

        public ObservableCollection<LabelValuePair> Column1Items { get; private set; }

        static readonly PropertyChangedEventArgs Column1ItemsChangedEventArgs = ObservableHelper.CreateArgs<AboutViewModel>(x => x.Column1Items);

        #endregion

        #region public ObservableCollection<LabelValuePair> Column2Items { get; set; }

        public ObservableCollection<LabelValuePair> Column2Items { get; private set; }

        static readonly PropertyChangedEventArgs Column2ItemsChangedEventArgs = ObservableHelper.CreateArgs<AboutViewModel>(x => x.Column2Items);

        #endregion

        #region public string MapDllVersion { get; set; }

        public string MapDllVersion
        {
            get { return _mapDllVersion; }
            set
            {
                if (_mapDllVersion == value) return;
                _mapDllVersion = value;
                Column1Items.Add(new LabelValuePair
                                 {
                                     Label = "Map Control DLL version",
                                     Value = MapDllVersion,
                                 });
                NotifyPropertyChanged(Column1ItemsChangedEventArgs);
            }
        }

        string _mapDllVersion = "Unknown";

        #endregion

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
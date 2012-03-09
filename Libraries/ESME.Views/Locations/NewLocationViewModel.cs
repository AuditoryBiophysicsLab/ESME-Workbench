using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Windows.Data;
using Cinch;
using ESME.Plugins;
using HRC.Collections;
using System.Linq;
using HRC.Validation;

namespace ESME.Views.Locations
{
    public sealed class NewLocationViewModel : ValidatingViewModel
    {
        #region Constructor
        public NewLocationViewModel(IPluginManagerService pluginManagerService)
        {
            _pluginManagerService = pluginManagerService;
            EnvironmentDataSourceViews = new Dictionary<PluginSubtype, ICollectionView>();
            SelectedPlugins = new ObservableConcurrentDictionary<PluginSubtype, EnvironmentalDataSourcePluginBase>();
            SelectedPlugins.CollectionChanged += (s, e) =>
            {
                switch (e.Action)
                {
                    case NotifyCollectionChangedAction.Add:
                        break;
                    case NotifyCollectionChangedAction.Replace:
                        var newItem = (KeyValuePair<PluginSubtype, EnvironmentalDataSourcePluginBase>)e.NewItems[e.NewItems.Count - 1];
                        PluginManager.ESMEPluginDictionary[PluginType.EnvironmentalDataSource][newItem.Key].DefaultPlugin = newItem.Value;
                        break;
                    default:
                        throw new NotImplementedException(string.Format("The CollectionChanged action {0} is not implemented here", e.Action));
                }
            };
            
            AddEnvironmentDataSourceView(PluginSubtype.Wind);
            AddEnvironmentDataSourceView(PluginSubtype.SoundSpeed);
            AddEnvironmentDataSourceView(PluginSubtype.Sediment);
            AddEnvironmentDataSourceView(PluginSubtype.Bathymetry);
            ValidationRules.AddRange(new List<ValidationRule>
            {
                NorthValidationRule, SouthValidationRule, EastValidationRule, WestValidationRule
            });

        }
        #endregion
        #region PluginManager stuff
        readonly IPluginManagerService _pluginManagerService;
        public IPluginManagerService PluginManager { get { return _pluginManagerService; } }
        public Dictionary<PluginSubtype, ICollectionView> EnvironmentDataSourceViews { get; set; }
        public ObservableConcurrentDictionary<PluginSubtype, EnvironmentalDataSourcePluginBase> SelectedPlugins { get; set; }

        void AddEnvironmentDataSourceView(PluginSubtype pluginSubtype)
        {
            var curView = CollectionViewSource.GetDefaultView(PluginManager.ESMEPluginDictionary[PluginType.EnvironmentalDataSource][pluginSubtype].Values);
            ((ListCollectionView)curView).SortDescriptions.Add(new SortDescription("PluginName", ListSortDirection.Ascending));
            ((ListCollectionView)curView).Filter = p => ((IESMEPlugin)p).IsSelectable;
            EnvironmentDataSourceViews.Add(pluginSubtype, curView);
            var defaultPlugin = PluginManager.ESMEPluginDictionary[PluginType.EnvironmentalDataSource][pluginSubtype].DefaultPlugin ??
                                PluginManager.ESMEPluginDictionary[PluginType.EnvironmentalDataSource][pluginSubtype].Values.FirstOrDefault();
            SelectedPlugins.Add(pluginSubtype, (EnvironmentalDataSourcePluginBase)defaultPlugin);
        }
        #endregion

        #region public string LocationName { get; set; }

        public string LocationName
        {
            get { return _locationName; }
            set
            {
                if (_locationName == value) return;
                _locationName = value;
                NotifyPropertyChanged(LocationNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LocationNameChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.LocationName);
        string _locationName;

        #endregion
        #region public double North { get; set; }

        public double North
        {
            get { return _north; }
            set
            {
                _north = value;
                NotifyPropertyChanged(NorthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NorthChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.North);
        double _north;
        static readonly ValidationRule NorthValidationRule = new ValidationRule
        {
            PropertyName = "North",
            Description = "Must be between -90 and +90 and be greater than South",
            RuleDelegate = (o, r) =>
            {
                var target = (NewLocationViewModel)o;
                return target.North >= -90 && target.North <= 90 && target.North > target.South;
            },
        };

        #endregion
        #region public double South { get; set; }

        public double South
        {
            get { return _south; }
            set
            {
                _south = value;
                NotifyPropertyChanged(SouthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SouthChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.South);
        double _south;

        static readonly ValidationRule SouthValidationRule = new ValidationRule
        {
            PropertyName = "South",
            Description = "Must be between -90 and +90 and be less than North",
            RuleDelegate = (sender, eventArgs) =>
            {
                var target = (NewLocationViewModel)sender;
                return target.South >= -90 && target.South <= 90 && target.North > target.South;
            },
        };

        #endregion
        #region public double East { get; set; }

        public double East
        {
            get { return _east; }
            set
            {
                _east = value;
                NotifyPropertyChanged(EastChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs EastChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.East);
        double _east;

        static readonly ValidationRule EastValidationRule = new ValidationRule
        {
            PropertyName = "East",
            Description = "Must be between -180 and +180 and be greater than West",
            RuleDelegate = (sender, eventArgs) =>
            {
                var target = (NewLocationViewModel)sender;
                return target.East >= -180 && target.East <= 180 && target.East > target.West;
            },
        };

        #endregion
        #region public double West { get; set; }

        public double West
        {
            get { return _west; }
            set
            {
                _west = value;
                NotifyPropertyChanged(WestChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WestChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.West);
        double _west;

        static readonly ValidationRule WestValidationRule = new ValidationRule
        {
            PropertyName = "West",
            Description = "Must be between -180 and +180 and be less than East",
            RuleDelegate = (sender, eventArgs) =>
            {
                var target = (NewLocationViewModel)sender;
                return target.West >= -180 && target.West <= 180 && target.East > target.West;
            },
        };

        #endregion

        #region Commands
        #region OkCommand
        public SimpleCommand<object, object> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, object>(delegate { return IsValid; }, delegate { OkHandler(); })); }
        }

        SimpleCommand<object, object> _ok;

        void OkHandler()
        {
            Globals.AppSettings.Save();
            CloseActivePopUpCommand.Execute(true);
        }
        #endregion

        #region CancelCommand
        public SimpleCommand<object, object> CancelCommand
        {
            get { return _cancel ?? (_cancel = new SimpleCommand<object, object>(delegate { CancelHandler(); })); }
        }

        SimpleCommand<object, object> _cancel;

        void CancelHandler()
        {
            CloseActivePopUpCommand.Execute(true);
        }
        #endregion
        #endregion
    }
}

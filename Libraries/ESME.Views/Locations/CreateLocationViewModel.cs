﻿using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Windows.Data;
using Cinch;
using ESME.Environment.NAVO;
using ESME.Locations;
using ESME.Plugins;
using HRC.Aspects;
using HRC.Collections;
using System.Linq;
using HRC.Navigation;
using HRC.Validation;

namespace ESME.Views.Locations
{
    [NotifyPropertyChanged]
    public sealed class CreateLocationViewModel : ValidatingViewModel
    {
        #region Constructor
        public CreateLocationViewModel(IPluginManagerService plugins, MasterDatabaseService database, EnvironmentalCacheService cache)
        {
            _plugins = plugins;
            _database = database;
            _cache = cache;
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
                        PluginManager[PluginType.EnvironmentalDataSource, newItem.Key] = newItem.Value;
                        break;
                    default:
                        throw new NotImplementedException(string.Format("The CollectionChanged action {0} is not implemented here", e.Action));
                }
            };
            
            AddEnvironmentDataSourceView(PluginSubtype.Wind);
            AddEnvironmentDataSourceView(PluginSubtype.SoundSpeed);
            AddEnvironmentDataSourceView(PluginSubtype.Sediment);
            AddEnvironmentDataSourceView(PluginSubtype.Bathymetry);
            ValidationRules.AddRange(new List<ValidationRule> { NorthValidationRule, SouthValidationRule, EastValidationRule, WestValidationRule });
        }

        readonly EnvironmentalCacheService _cache;
        readonly MasterDatabaseService _database;
        #endregion
        #region PluginManager stuff
        readonly IPluginManagerService _plugins;
        public IPluginManagerService PluginManager { get { return _plugins; } }
        public Dictionary<PluginSubtype, ICollectionView> EnvironmentDataSourceViews { get; set; }
        public ObservableConcurrentDictionary<PluginSubtype, EnvironmentalDataSourcePluginBase> SelectedPlugins { get; set; }

        void AddEnvironmentDataSourceView(PluginSubtype pluginSubtype)
        {
            var curView = CollectionViewSource.GetDefaultView(PluginManager[PluginType.EnvironmentalDataSource][pluginSubtype].Values);
            ((ListCollectionView)curView).SortDescriptions.Add(new SortDescription("PluginName", ListSortDirection.Ascending));
            ((ListCollectionView)curView).Filter = p => ((IESMEPlugin)p).IsSelectable;
            EnvironmentDataSourceViews.Add(pluginSubtype, curView);
            var defaultPlugin = PluginManager[PluginType.EnvironmentalDataSource, pluginSubtype] ??
                                PluginManager[PluginType.EnvironmentalDataSource][pluginSubtype].Values.FirstOrDefault();
            SelectedPlugins.Add(pluginSubtype, (EnvironmentalDataSourcePluginBase)defaultPlugin);
        }
        #endregion

        public string LocationName { get; set; }
        public string Comments { get; set; }
        public double North { get; set; }
        public double South { get; set; }
        public double East { get; set; }
        public double West { get; set; }
        #region Validation Rules
        static readonly ValidationRule NorthValidationRule = new ValidationRule
        {
            PropertyName = "North",
            Description = "Must be between -90 and +90 and be greater than South",
            RuleDelegate = (o, r) =>
            {
                var target = (CreateLocationViewModel)o;
                return target.North >= -90 && target.North <= 90 && target.North > target.South;
            },
        };
        static readonly ValidationRule SouthValidationRule = new ValidationRule
        {
            PropertyName = "South",
            Description = "Must be between -90 and +90 and be less than North",
            RuleDelegate = (sender, eventArgs) =>
            {
                var target = (CreateLocationViewModel)sender;
                return target.South >= -90 && target.South <= 90 && target.North > target.South;
            },
        };
        static readonly ValidationRule EastValidationRule = new ValidationRule
        {
            PropertyName = "East",
            Description = "Must be between -180 and +180 and be greater than West",
            RuleDelegate = (sender, eventArgs) =>
            {
                var target = (CreateLocationViewModel)sender;
                return target.East >= -180 && target.East <= 180 && target.East > target.West;
            },
        };
        static readonly ValidationRule WestValidationRule = new ValidationRule
        {
            PropertyName = "West",
            Description = "Must be between -180 and +180 and be less than East",
            RuleDelegate = (sender, eventArgs) =>
            {
                var target = (CreateLocationViewModel)sender;
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
            var location = new Location
            {
                Name = LocationName,
                Comments = Comments,
                GeoRect = new GeoRect(North, South, East, West),
            };
            _database.Add(location, true);
            foreach (var dataSet in from pluginSubtype in new[] { PluginSubtype.Wind, PluginSubtype.SoundSpeed, PluginSubtype.Sediment, PluginSubtype.Bathymetry }
                                    where SelectedPlugins[pluginSubtype] != null
                                    from dataSet in SelectedPlugins[pluginSubtype].SelectedDataSets
                                    select dataSet)
            {
                dataSet.Location = location;
                dataSet.FileName = Path.GetFileNameWithoutExtension(Path.GetRandomFileName()) + "." + ((PluginIdentifier)dataSet.SourcePlugin).PluginSubtype.ToString().ToLower();
                _database.Add(dataSet, true);
                _cache.ImportDataset(dataSet);
            }
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

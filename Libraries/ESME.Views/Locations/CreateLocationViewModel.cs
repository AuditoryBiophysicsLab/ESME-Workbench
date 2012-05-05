using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Windows;
using System.Windows.Data;
using ESME.Locations;
using ESME.Plugins;
using HRC.Collections;
using System.Linq;
using HRC.Validation;
using HRC.ViewModels;

namespace ESME.Views.Locations
{
    public sealed class CreateLocationViewModel : ValidatingViewModel
    {
        #region Constructor
        public CreateLocationViewModel(IPluginManagerService plugins, IMasterDatabaseService database, EnvironmentalCacheService cache, EditOverlayViewModel editOverlayViewModel)
        {
            _plugins = plugins;
            _database = database;
            _cache = cache;
            EditOverlayViewModel = editOverlayViewModel;
            EnvironmentDataSourceViews = new Dictionary<PluginSubtype, ICollectionView>();
            SelectedPlugins = new ObservableConcurrentDictionary<PluginSubtype, EnvironmentalDataSourcePluginBase>();
            
            AddEnvironmentDataSourceView(PluginSubtype.Wind);
            AddEnvironmentDataSourceView(PluginSubtype.SoundSpeed);
            AddEnvironmentDataSourceView(PluginSubtype.Sediment);
            AddEnvironmentDataSourceView(PluginSubtype.Bathymetry);
            ValidationRules.Add(LocationNameValidationRule);
        }

        readonly EnvironmentalCacheService _cache;
        readonly IMasterDatabaseService _database;
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

        public EditOverlayViewModel EditOverlayViewModel { get; private set; }

        public string LocationName { get; set; }
        #region Validation Rules
        static readonly ValidationRule LocationNameValidationRule = new ValidationRule
        {
            PropertyName = "LocationName",
            Description = "Must be unique and cannot be null or empty",
            RuleDelegate = (o, r) =>
            {
                var target = (CreateLocationViewModel)o;
                return !string.IsNullOrEmpty(target.LocationName);
            },
        };
        #endregion

        public string Comments { get; set; }

        #region Commands
        #region OkCommand
        public SimpleCommand<object, object> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, object>(delegate { return IsValid; }, delegate { OkHandler(); })); }
        }

        SimpleCommand<object, object> _ok;

        void OkHandler()
        {
            EditOverlayViewModel.IsVisible = false;
            var location = new Location
            {
                Name = LocationName,
                Comments = Comments,
                GeoRect = EditOverlayViewModel.GeoRect
            };
            _database.Add(location, true);
#if false
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
#endif
            Globals.AppSettings.Save();
            Window.Close();
        }
        #endregion

        public Window Window { get; set; }

        #region CancelCommand
        public SimpleCommand<object, object> CancelCommand
        {
            get
            {
                return _cancel ?? (_cancel = new SimpleCommand<object, object>(o =>
                {
                    EditOverlayViewModel.IsVisible = false;
                    Window.Close();
                }));
            }
        }
        SimpleCommand<object, object> _cancel;

        #endregion
        #endregion
    }
}

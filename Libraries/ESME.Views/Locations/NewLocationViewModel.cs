using System.Collections.Generic;
using System.ComponentModel;
using System.Windows.Data;
using Cinch;
using ESME.Plugins;

namespace ESME.Views.Locations
{
    public class NewLocationViewModel : ViewModelBase
    {
        public NewLocationViewModel(IPluginManagerService pluginManagerService)
        {
            _pluginManagerService = pluginManagerService;
            ConfiguredEnvironmentDataSourceViews = new Dictionary<PluginSubtype, ICollectionView>();
            SelectedPlugins = new Dictionary<PluginSubtype, EnvironmentalDataSourcePluginBase>();

            AddConfiguredEnvironmentDataSourceView(PluginSubtype.Wind);
            AddConfiguredEnvironmentDataSourceView(PluginSubtype.SoundSpeed);
            AddConfiguredEnvironmentDataSourceView(PluginSubtype.Sediment);
            AddConfiguredEnvironmentDataSourceView(PluginSubtype.Bathymetry);
        }

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

        #endregion

        readonly IPluginManagerService _pluginManagerService;
        public IPluginManagerService PluginManager { get { return _pluginManagerService; } }
        public Dictionary<PluginSubtype, ICollectionView> ConfiguredEnvironmentDataSourceViews { get; set; }
        public Dictionary<PluginSubtype, EnvironmentalDataSourcePluginBase> SelectedPlugins { get; set; }

        void AddConfiguredEnvironmentDataSourceView(PluginSubtype pluginSubtype)
        {
            var curView = CollectionViewSource.GetDefaultView(PluginManager.ESMEPluginDictionary[PluginType.EnvironmentalDataSource][pluginSubtype].Values);
            ((ListCollectionView)curView).SortDescriptions.Add(new SortDescription("PluginName", ListSortDirection.Ascending));
            ((ListCollectionView)curView).Filter = p => ((IESMEPlugin)p).IsConfigured;
            ConfiguredEnvironmentDataSourceViews.Add(pluginSubtype, curView);
            var defaultPlugin = PluginManager.ESMEPluginDictionary[PluginType.EnvironmentalDataSource][pluginSubtype].DefaultPlugin;
            if (!defaultPlugin.IsSelectable) defaultPlugin = null;
            SelectedPlugins.Add(pluginSubtype, (EnvironmentalDataSourcePluginBase)defaultPlugin);
        }
    }
}

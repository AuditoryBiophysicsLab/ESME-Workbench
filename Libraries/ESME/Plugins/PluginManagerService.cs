using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.ComponentModel.Composition.Hosting;
using System.ComponentModel.Composition.Primitives;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using ESME.Environment;
using HRC;
using HRC.Collections;
using HRC.Composition;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Plugins
{
    public interface IPluginManagerService
    {
        ESMEPluginDictionary ESMEPluginDictionary { get; }
        List<PluginIdentifier> DefaultPluginIdentifiers { get; set; }
        EnvironmentalDataSourcePluginBase<Wind> WindSource { get; }
        EnvironmentalDataSourcePluginBase<SoundSpeed> SoundSpeedSource { get; }
        EnvironmentalDataSourcePluginBase<Sediment> SedimentSource { get; }
        EnvironmentalDataSourcePluginBase<Bathymetry> BathymetrySource { get; }
    }

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(IPluginManagerService))]
    public class PluginManagerService : IPluginManagerService
    {
        public PluginManagerService() 
        {
            _esmePlugins = new List<IESMEPlugin>();
            ESMEPluginDictionary = new ESMEPluginDictionary();
            var catalog = new ESMEPluginCatalog(new DirectoryCatalog(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)));
            var container = new CompositionContainer(catalog, true);
            try
            {
                container.ComposeParts(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine(ex.Message);
            }
            if (_esmePlugins == null || _esmePlugins.Count <= 0) return;
            foreach (var plugin in _esmePlugins)
            {
                if (!ESMEPluginDictionary.ContainsKey(plugin.PluginType)) ESMEPluginDictionary.Add(plugin.PluginType, new PluginTypeDictionary());
                if (!ESMEPluginDictionary[plugin.PluginType].ContainsKey(plugin.PluginSubtype)) ESMEPluginDictionary[plugin.PluginType].Add(plugin.PluginSubtype, new PluginSubtypeDictionary());
                ESMEPluginDictionary[plugin.PluginType][plugin.PluginSubtype][plugin.GetType().ToString()] = plugin;
                plugin.LoadSettings();
            }
        }
        [ImportMany, UsedImplicitly] ICollection<IESMEPlugin> _esmePlugins;

        public ESMEPluginDictionary ESMEPluginDictionary { get; private set; }
        public List<PluginIdentifier> DefaultPluginIdentifiers
        {
            get
            {
                return new List<PluginIdentifier>(from pluginType in ESMEPluginDictionary.Keys
                                                  from subtype in ESMEPluginDictionary[pluginType].Keys
                                                  where ESMEPluginDictionary[pluginType][subtype].DefaultPlugin != null
                                                  select ESMEPluginDictionary[pluginType][subtype].DefaultPlugin.PluginIdentifier);
            }
            set
            {
                value.ForEach(configuration => ESMEPluginDictionary.PluginIdentifier = configuration);
            }
        }

        public IESMEPlugin this[PluginType pluginType, PluginSubtype pluginSubtype]
        {
            get
            {
                if (!ESMEPluginDictionary.ContainsKey(pluginType)) return null;
                return !ESMEPluginDictionary[pluginType].ContainsKey(pluginSubtype) ? null : ESMEPluginDictionary[pluginType][pluginSubtype].DefaultPlugin;
            }
            set
            {
                if (!ESMEPluginDictionary.ContainsKey(pluginType)) ESMEPluginDictionary.Add(pluginType, new PluginTypeDictionary());
                if (!ESMEPluginDictionary[pluginType].ContainsKey(pluginSubtype)) ESMEPluginDictionary[pluginType].Add(pluginSubtype, new PluginSubtypeDictionary());
                ESMEPluginDictionary[pluginType][pluginSubtype].DefaultPlugin = value;
            }
        }

        public PluginTypeDictionary this[PluginType pluginType]
        {
            get
            {
                return !ESMEPluginDictionary.ContainsKey(pluginType) ? null : ESMEPluginDictionary[pluginType];
            }
        }

        public EnvironmentalDataSourcePluginBase<Wind> WindSource
        {
            get { return (EnvironmentalDataSourcePluginBase<Wind>)this[PluginType.EnvironmentalDataSource, PluginSubtype.Wind]; }
            set { this[PluginType.EnvironmentalDataSource, PluginSubtype.Wind] = value; }
        }

        public EnvironmentalDataSourcePluginBase<SoundSpeed> SoundSpeedSource
        {
            get { return (EnvironmentalDataSourcePluginBase<SoundSpeed>)this[PluginType.EnvironmentalDataSource, PluginSubtype.SoundSpeed]; }
            set { this[PluginType.EnvironmentalDataSource, PluginSubtype.SoundSpeed] = value; }
        }

        public EnvironmentalDataSourcePluginBase<Sediment> SedimentSource
        {
            get { return (EnvironmentalDataSourcePluginBase<Sediment>)this[PluginType.EnvironmentalDataSource, PluginSubtype.Sediment]; }
            set { this[PluginType.EnvironmentalDataSource, PluginSubtype.Sediment] = value; }
        }

        public EnvironmentalDataSourcePluginBase<Bathymetry> BathymetrySource
        {
            get { return (EnvironmentalDataSourcePluginBase<Bathymetry>)this[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry]; }
            set { this[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry] = value; }
        }
    }

    public class PluginSubtypeDictionary : ObservableConcurrentDictionary<string, IESMEPlugin>
    {
        IESMEPlugin _defaultPlugin;
        public IESMEPlugin DefaultPlugin
        {
            get
            {
                return _defaultPlugin;
            }
            set
            {
                _defaultPlugin = value;
            }
        }
    }
    public class PluginTypeDictionary : ObservableConcurrentDictionary<PluginSubtype, PluginSubtypeDictionary> {}
    public class ESMEPluginDictionary : ObservableConcurrentDictionary<PluginType, PluginTypeDictionary>
    {
        public PluginIdentifier PluginIdentifier
        {
            set
            {
                if (!ContainsKey(value.PluginType)) return;
                if (!this[value.PluginType].ContainsKey(value.PluginSubtype)) return;
                if (!this[value.PluginType][value.PluginSubtype].ContainsKey(value.Type)) return;
                this[value.PluginType][value.PluginSubtype].DefaultPlugin = this[value.PluginType][value.PluginSubtype][value.Type];
            }
        }
    }

    public class ESMEPluginCatalog : FilteredCatalog
    {
        public ESMEPluginCatalog(ComposablePartCatalog catalogToFilter) : base(catalogToFilter) { }
        protected override bool IsMatch(ExportDefinition exportDefinition)
        {
            return !string.IsNullOrEmpty(exportDefinition.ContractName) && exportDefinition.ContractName == "ESME.IESMEPlugin";
        }
    }

    [Serializable]
    public class PluginIdentifier
    {
        public PluginIdentifier() {}
        public PluginIdentifier(PluginIdentifier pluginIdentifier) 
        {
            PluginType = pluginIdentifier.PluginType;
            PluginSubtype = pluginIdentifier.PluginSubtype;
            Type = pluginIdentifier.Type;
        }

        public PluginType PluginType { get; set; }
        public PluginSubtype PluginSubtype { get; set; }
        public string Type { get; set; }
    }
}

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
        List<DefaultPluginConfiguration> DefaultPluginConfigurations { get; set; }
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
            var catalog = new ESMEPluginCatalog(new DirectoryCatalog(Path.GetDirectoryName(Assembly.GetCallingAssembly().Location)));
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
                if (!ESMEPluginDictionary[plugin.PluginType].ContainsKey(plugin.Subtype)) ESMEPluginDictionary[plugin.PluginType].Add(plugin.Subtype, new PluginSubtypeDictionary());
                ESMEPluginDictionary[plugin.PluginType][plugin.Subtype][plugin.GetType().ToString()] = plugin;
                plugin.LoadSettings();
            }
        }
        [ImportMany, UsedImplicitly] ICollection<IESMEPlugin> _esmePlugins;

        public ESMEPluginDictionary ESMEPluginDictionary { get; private set; }
        public List<DefaultPluginConfiguration> DefaultPluginConfigurations
        {
            get
            {
                return new List<DefaultPluginConfiguration>(from pluginType in ESMEPluginDictionary.Keys
                                                            from subtype in ESMEPluginDictionary[pluginType].Keys
                                                            where ESMEPluginDictionary[pluginType][subtype].DefaultPlugin != null
                                                            select new DefaultPluginConfiguration
                                                            {
                                                                PluginType = pluginType,
                                                                Subtype = subtype,
                                                                Type = ESMEPluginDictionary[pluginType][subtype].DefaultPlugin.GetType().ToString(),
                                                            });
            }
            set
            {
                value.ForEach(configuration => ESMEPluginDictionary.DefaultPluginConfiguration = configuration);
            }
        }

        public IESMEPlugin this[PluginType pluginType, string subType]
        {
            get
            {
                if (!ESMEPluginDictionary.ContainsKey(pluginType)) return null;
                return !ESMEPluginDictionary[pluginType].ContainsKey(subType) ? null : ESMEPluginDictionary[pluginType][subType].DefaultPlugin;
            }
            set
            {
                if (!ESMEPluginDictionary.ContainsKey(pluginType)) ESMEPluginDictionary.Add(pluginType, new PluginTypeDictionary());
                if (!ESMEPluginDictionary[pluginType].ContainsKey(subType)) ESMEPluginDictionary[pluginType].Add(subType, new PluginSubtypeDictionary());
                ESMEPluginDictionary[pluginType][subType].DefaultPlugin = value;
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
            get { return (EnvironmentalDataSourcePluginBase<Wind>)this[PluginType.EnvironmentalDataSource, "Wind"]; }
            set { this[PluginType.EnvironmentalDataSource, "Wind"] = value; }
        }

        public EnvironmentalDataSourcePluginBase<SoundSpeed> SoundSpeedSource
        {
            get { return (EnvironmentalDataSourcePluginBase<SoundSpeed>)this[PluginType.EnvironmentalDataSource, "Sound Speed"]; }
            set { this[PluginType.EnvironmentalDataSource, "Sound Speed"] = value; }
        }

        public EnvironmentalDataSourcePluginBase<Sediment> SedimentSource
        {
            get { return (EnvironmentalDataSourcePluginBase<Sediment>)this[PluginType.EnvironmentalDataSource, "Sediment"]; }
            set { this[PluginType.EnvironmentalDataSource, "Sediment"] = value; }
        }

        public EnvironmentalDataSourcePluginBase<Bathymetry> BathymetrySource
        {
            get { return (EnvironmentalDataSourcePluginBase<Bathymetry>)this[PluginType.EnvironmentalDataSource, "Bathymetry"]; }
            set { this[PluginType.EnvironmentalDataSource, "Bathymetry"] = value; }
        }
    }

    public class PluginSubtypeDictionary : ObservableConcurrentDictionary<string, IESMEPlugin>
    {
        IESMEPlugin _defaultPlugin;
        public IESMEPlugin DefaultPlugin
        {
            get
            {
                if (_defaultPlugin != null && _defaultPlugin.IsSelectable) return _defaultPlugin;
                return (from plugin in Values where plugin.PluginName == "None Selected" select plugin).FirstOrDefault();
            }
            set { _defaultPlugin = value; }
        }
    }
    public class PluginTypeDictionary : ObservableConcurrentDictionary<string, PluginSubtypeDictionary> {}
    public class ESMEPluginDictionary : ObservableConcurrentDictionary<PluginType, PluginTypeDictionary>
    {
        public DefaultPluginConfiguration DefaultPluginConfiguration
        {
            set
            {
                if (!ContainsKey(value.PluginType)) return;
                if (!this[value.PluginType].ContainsKey(value.Subtype)) return;
                if (!this[value.PluginType][value.Subtype].ContainsKey(value.Type)) return;
                this[value.PluginType][value.Subtype].DefaultPlugin = this[value.PluginType][value.Subtype][value.Type];
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
    public class DefaultPluginConfiguration
    {
        public PluginType PluginType { get; set; }
        public string Subtype { get; set; }
        public string Type { get; set; }
    }
}

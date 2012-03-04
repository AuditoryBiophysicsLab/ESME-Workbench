using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.ComponentModel.Composition.Hosting;
using System.ComponentModel.Composition.Primitives;
using System.Diagnostics;
using System.Linq;
using HRC;
using HRC.Collections;
using HRC.Composition;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Plugins
{
    public interface IPluginManagerService
    {
        ESMEPluginDictionary ESMEPluginDictionary { get; }
    }

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(IPluginManagerService))]
    public class PluginManagerService : IPluginManagerService
    {
        public PluginManagerService() 
        {
            _esmePlugins = new List<IESMEPlugin>();
            ESMEPluginDictionary = new ESMEPluginDictionary();
            var catalog = new ESMEPluginCatalog(new DirectoryCatalog(@"C:\Projects\ESME Deliverables\Plugins\Environmental Data Sources\InstallableNAVO\bin\Debug"));
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
            }
        }
        [ImportMany, UsedImplicitly] ICollection<IESMEPlugin> _esmePlugins;

        public ESMEPluginDictionary ESMEPluginDictionary { get; private set; }
    }

    public class PluginSubtypeDictionary : ObservableConcurrentDictionary<string, IESMEPlugin>
    {
        public IESMEPlugin DefaultPlugin { get; set; }
    }
    public class PluginTypeDictionary : ObservableConcurrentDictionary<string, PluginSubtypeDictionary> {}
    public class ESMEPluginDictionary : ObservableConcurrentDictionary<PluginType, PluginTypeDictionary>
    {
        public bool SetDefaultPluginConfiguration(DefaultPluginConfiguration configuration)
        {
            if (!ContainsKey(configuration.PluginType)) return false;
            if (!this[configuration.PluginType].ContainsKey(configuration.Subtype)) return false;
            if (!this[configuration.PluginType][configuration.Subtype].ContainsKey(configuration.Type)) return false;
            this[configuration.PluginType][configuration.Subtype].DefaultPlugin = this[configuration.PluginType][configuration.Subtype][configuration.Type];
            return true;
        }
        
        public void SetDefaultPluginConfigurations(IList<DefaultPluginConfiguration> configurations)
        {
            var rejects = configurations.Where(configuration => !SetDefaultPluginConfiguration(configuration)).ToList();
            foreach (var reject in rejects) configurations.Remove(reject);
        }

        public List<DefaultPluginConfiguration> GetDefaultPluginConfigurations()
        {
            return new List<DefaultPluginConfiguration>(from pluginType in Keys
                                                        from subtype in this[pluginType].Keys
                                                        where this[pluginType][subtype].DefaultPlugin != null
                                                        select new DefaultPluginConfiguration
                                                        {
                                                            PluginType = pluginType,
                                                            Subtype = subtype,
                                                            Type = this[pluginType][subtype].DefaultPlugin.GetType().ToString(),
                                                        });
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

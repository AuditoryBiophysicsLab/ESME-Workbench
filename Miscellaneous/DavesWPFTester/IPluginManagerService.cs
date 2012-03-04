using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.ComponentModel.Composition.Hosting;
using System.ComponentModel.Composition.Primitives;
using System.Diagnostics;
using ESME;
using HRC;
using HRC.Collections;
using HRC.Composition;
using HRC.Utility;
using MEFedMVVM.ViewModelLocator;

namespace DavesWPFTester
{
    public interface IPluginManagerService
    {
        ObservableList<IESMEPlugin> ESMEPlugins { get; }
        ESMEPluginDictionary ESMEPluginDictionary { get; }
    }

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(IPluginManagerService))]
    public class PluginManagerService : IPluginManagerService
    {
        public PluginManagerService() 
        {
            _esmePlugins = new List<IESMEPlugin>();
            ESMEPlugins = new ObservableList<IESMEPlugin>();
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
                ESMEPlugins.Add(plugin);
                if (!ESMEPluginDictionary.ContainsKey(plugin.PluginType)) ESMEPluginDictionary.Add(plugin.PluginType, new PluginTypeDictionary());
                if (!ESMEPluginDictionary[plugin.PluginType].ContainsKey(plugin.Subtype)) ESMEPluginDictionary[plugin.PluginType].Add(plugin.Subtype, new PluginSubtypeDictionary());
                ESMEPluginDictionary[plugin.PluginType][plugin.Subtype][plugin.GetType().ToString()] = plugin;
            }
        }
        [ImportMany, UsedImplicitly] ICollection<IESMEPlugin> _esmePlugins;

        public ObservableList<IESMEPlugin> ESMEPlugins { get; set; }
        public ESMEPluginDictionary ESMEPluginDictionary { get; private set; }
    }

    public class PluginSubtypeDictionary : ObservableConcurrentDictionary<string, IESMEPlugin>
    {
        public IESMEPlugin DefaultPlugin { get; set; }
    }
    public class PluginTypeDictionary : ObservableConcurrentDictionary<string, PluginSubtypeDictionary> { }
    public class ESMEPluginDictionary : ObservableConcurrentDictionary<PluginType, PluginTypeDictionary> {}

    public class ESMEPluginCatalog : FilteredCatalog
    {
        public ESMEPluginCatalog(ComposablePartCatalog catalogToFilter) : base(catalogToFilter) { }
        protected override bool IsMatch(ExportDefinition exportDefinition)
        {
            return !string.IsNullOrEmpty(exportDefinition.ContractName) && exportDefinition.ContractName == "ESME.IESMEPlugin";
        }
    }
}

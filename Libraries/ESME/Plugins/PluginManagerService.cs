using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.ComponentModel.Composition.Hosting;
using System.ComponentModel.Composition.Primitives;
using System.Diagnostics;
using System.IO;
using System.Linq;
using HRC;
using HRC.Collections;
using HRC.Composition;
using MEFedMVVM.ViewModelLocator;
using FilteredCatalog = HRC.Composition.FilteredCatalog;

namespace ESME.Plugins
{
    public interface IPluginManagerService
    {
        string PluginDirectory { get; set; }
        List<PluginIdentifier> DefaultPluginIdentifiers { get; set; }
        PluginTypeDictionary this[PluginType pluginType] { get; }
        IESMEPlugin this[PluginType pluginType, PluginSubtype pluginSubtype] { get; set; }
        IESMEPlugin this[PluginIdentifier pluginIdentifier] { get; }
    }

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(IPluginManagerService))]
    public class PluginManagerService : IPluginManagerService
    {
        public PluginManagerService() 
        {
            _esmePlugins = new List<IESMEPlugin>();
            _esmePluginDictionary = new ESMEPluginDictionary();
        }

        [ImportMany, UsedImplicitly] ICollection<IESMEPlugin> _esmePlugins;

        string _pluginDirectory;
        public string PluginDirectory
        {
            get { return _pluginDirectory; }
            set
            {
                _pluginDirectory = value;
                if (string.IsNullOrEmpty(_pluginDirectory)) throw new ApplicationException("PluginManager.PluginDirectory cannot be null or empty");
                if (!Directory.Exists(_pluginDirectory)) throw new ApplicationException(string.Format("PluginManager: Specified plugin directory \"{0}\" does not exist", _pluginDirectory));

                var catalog = new ESMEPluginCatalog(new DirectoryCatalog(_pluginDirectory));
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
                    if (!_esmePluginDictionary.ContainsKey(plugin.PluginType)) _esmePluginDictionary.Add(plugin.PluginType, new PluginTypeDictionary());
                    if (!_esmePluginDictionary[plugin.PluginType].ContainsKey(plugin.PluginSubtype)) _esmePluginDictionary[plugin.PluginType].Add(plugin.PluginSubtype, new PluginSubtypeDictionary());
                    _esmePluginDictionary[plugin.PluginType][plugin.PluginSubtype][plugin.GetType().ToString()] = plugin;
                    plugin.LoadSettings();
                }
            }
        }

        readonly ESMEPluginDictionary _esmePluginDictionary;

        public List<PluginIdentifier> DefaultPluginIdentifiers
        {
            get
            {
                return new List<PluginIdentifier>(from pluginType in _esmePluginDictionary.Keys
                                                  from subtype in _esmePluginDictionary[pluginType].Keys
                                                  where _esmePluginDictionary[pluginType][subtype].DefaultPlugin != null
                                                  select _esmePluginDictionary[pluginType][subtype].DefaultPlugin.PluginIdentifier);
            }
            set
            {
                if (value == null) return;
                value.ForEach(configuration => _esmePluginDictionary.PluginIdentifier = configuration);
            }
        }

        public IESMEPlugin this[PluginType pluginType, PluginSubtype pluginSubtype]
        {
            get
            {
                if (!_esmePluginDictionary.ContainsKey(pluginType)) return null;
                return !_esmePluginDictionary[pluginType].ContainsKey(pluginSubtype) ? null : _esmePluginDictionary[pluginType][pluginSubtype].DefaultPlugin;
            }
            set
            {
                if (!_esmePluginDictionary.ContainsKey(pluginType)) _esmePluginDictionary.Add(pluginType, new PluginTypeDictionary());
                if (!_esmePluginDictionary[pluginType].ContainsKey(pluginSubtype)) _esmePluginDictionary[pluginType].Add(pluginSubtype, new PluginSubtypeDictionary());
                _esmePluginDictionary[pluginType][pluginSubtype].DefaultPlugin = value;
            }
        }

        public IESMEPlugin this[PluginIdentifier pluginIdentifier]
        {
            get
            {
                if (!_esmePluginDictionary.ContainsKey(pluginIdentifier.PluginType)) throw new PluginNotFoundException(string.Format("There are no plugins of type {0}", pluginIdentifier.PluginType));
                if (!_esmePluginDictionary[pluginIdentifier.PluginType].ContainsKey(pluginIdentifier.PluginSubtype)) throw new PluginNotFoundException(string.Format("There are no {0} plugins of subtype {1}", pluginIdentifier.PluginType, pluginIdentifier.PluginSubtype));
                if (!_esmePluginDictionary[pluginIdentifier.PluginType][pluginIdentifier.PluginSubtype].ContainsKey(pluginIdentifier.Type)) throw new PluginNotFoundException(string.Format("There are no {0}.{1} plugins of type {2}", pluginIdentifier.PluginType, pluginIdentifier.PluginSubtype, pluginIdentifier.Type));
                return _esmePluginDictionary[pluginIdentifier.PluginType][pluginIdentifier.PluginSubtype][pluginIdentifier.Type];
            }
        }

        public PluginTypeDictionary this[PluginType pluginType]
        {
            get
            {
                return !_esmePluginDictionary.ContainsKey(pluginType) ? null : _esmePluginDictionary[pluginType];
            }
        }
    }

    public class PluginSubtypeDictionary : ObservableConcurrentDictionary<string, IESMEPlugin>
    {
        IESMEPlugin _defaultPlugin;
        public IESMEPlugin DefaultPlugin
        {
            get { return _defaultPlugin ?? (_defaultPlugin = Values.FirstOrDefault(p => p.IsConfigured)); }
            set { _defaultPlugin = value; }
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
    public class PluginIdentifier : IEquatable<PluginIdentifier>
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
        public static bool operator==(PluginIdentifier x, PluginIdentifier y) { return x.Equals(y); }
        public static bool operator !=(PluginIdentifier x, PluginIdentifier y) { return !(x == y); }

        public bool Equals(PluginIdentifier other) 
        {
            if (ReferenceEquals(null, other)) return false;
            if (ReferenceEquals(this, other)) return true;
            return Equals(other.PluginType, PluginType) && Equals(other.PluginSubtype, PluginSubtype) && Equals(other.Type, Type);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj.GetType() == typeof(PluginIdentifier) && Equals((PluginIdentifier)obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var result = PluginType.GetHashCode();
                result = (result * 397) ^ PluginSubtype.GetHashCode();
                result = (result * 397) ^ (Type != null ? Type.GetHashCode() : 0);
                return result;
            }
        }
    }
}

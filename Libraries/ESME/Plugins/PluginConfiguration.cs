using System;
using System.Collections.Generic;
using HRC.Validation;

namespace ESME.Plugins
{
    [Serializable]
    public abstract class PluginConfiguration : ValidatingViewModel
    {
        public Type PluginType { get; set; }
        public string PluginName { get; set; }

        public static implicit operator KeyValuePair<Type, PluginConfiguration>(PluginConfiguration p) { return new KeyValuePair<Type, PluginConfiguration>(p.PluginType, p); }
    }

    public class PluginConfigurations : Dictionary<Type, PluginConfiguration> {}
}

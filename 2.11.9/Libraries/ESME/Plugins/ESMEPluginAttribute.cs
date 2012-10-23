using System;
using System.ComponentModel.Composition;
using ESME.Environment;

namespace ESME.Plugins
{
    [MetadataAttribute]
    [AttributeUsage(AttributeTargets.Class, AllowMultiple = false)]
    public class ESMEPluginAttribute : ExportAttribute
    {
        public ESMEPluginAttribute() : base(typeof(IESMEPlugin)) { }
        protected ESMEPluginAttribute(Type pluginType) : base(pluginType) { }

        public PluginType PluginType { get; set; }
        public PluginSubtype PluginSubtype { get; set; }
        public string Name { get; set; }
        public string Description { get; set; }
    }

    [MetadataAttribute]
    [AttributeUsage(AttributeTargets.Class, AllowMultiple = false)]
    public class EnvironmentDataSourceAttribute : ESMEPluginAttribute
    {
        public EnvironmentDataSourceAttribute() : base(typeof(IESMEPlugin)) { }

        public EnvironmentDataType EnvironmentDataType { get; set; }
    }
}
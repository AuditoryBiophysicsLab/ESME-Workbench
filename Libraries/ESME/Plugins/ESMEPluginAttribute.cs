using System;
using System.ComponentModel.Composition;
using ESME.Environment.Descriptors;

namespace ESME.Plugins
{
    [MetadataAttribute]
    [AttributeUsage(AttributeTargets.Class, AllowMultiple = false)]
    public class ESMEPluginAttribute : ExportAttribute
    {
        public ESMEPluginAttribute() : base(typeof(IESMEPlugin)) { }

        public PluginType PluginType { get; set; }
        public PluginSubtype PluginSubtype { get; set; }
        public EnvironmentDataType EnvironmentDataType { get; set; }
        public string Name { get; set; }
        public string Description { get; set; }
    }
}
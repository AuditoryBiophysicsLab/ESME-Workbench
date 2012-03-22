using System.ComponentModel.DataAnnotations;
using ESME.Plugins;

namespace ESME.Database
{
    [ComplexType]
    public class DbPluginIdentifier
    {
        public DbPluginIdentifier() {}
        public DbPluginIdentifier(PluginIdentifier pluginIdentifier)
        {
            PluginType = pluginIdentifier.PluginType;
            PluginSubtype = pluginIdentifier.PluginSubtype;
            Type = pluginIdentifier.Type;
        }

        public static implicit operator DbPluginIdentifier(PluginIdentifier pluginIdentifier) { return new DbPluginIdentifier(pluginIdentifier); }
        public static implicit operator PluginIdentifier(DbPluginIdentifier dbPluginIdentifier)
        {
            return new PluginIdentifier
            {
                PluginType = dbPluginIdentifier.PluginType,
                PluginSubtype = dbPluginIdentifier.PluginSubtype,
                Type = dbPluginIdentifier.Type,
            };
        }

        public PluginType PluginType { get; set; }
        public PluginSubtype PluginSubtype { get; set; }
        public string Type { get; set; }

        public override string ToString() { return string.Format("{0}:{1}:{2}", PluginType, PluginSubtype, Type); }
    }
}
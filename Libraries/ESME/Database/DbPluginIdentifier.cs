using System.ComponentModel.DataAnnotations;
using ESME.Plugins;

namespace ESME.Database
{
    [ComplexType]
    public class DbPluginIdentifier
    {
        public DbPluginIdentifier(PluginIdentifier pluginIdentifier) { _pluginIdentifier = new PluginIdentifier(pluginIdentifier); }

        public static implicit operator DbPluginIdentifier(PluginIdentifier pluginIdentifier) { return new DbPluginIdentifier(pluginIdentifier); }
        public static implicit operator PluginIdentifier(DbPluginIdentifier dbPluginIdentifier) { return new PluginIdentifier(dbPluginIdentifier._pluginIdentifier); }

        readonly PluginIdentifier _pluginIdentifier = new PluginIdentifier();

        public PluginType PluginType
        {
            get { return _pluginIdentifier.PluginType; }
            set { _pluginIdentifier.PluginType = value; }
        }
        public PluginSubtype PluginSubtype
        {
            get { return _pluginIdentifier.PluginSubtype; }
            set { _pluginIdentifier.PluginSubtype = value; }
        }
        public string Type
        {
            get { return _pluginIdentifier.Type; }
            set { _pluginIdentifier.Type = value; }
        }

        public override string ToString() { return string.Format("{0}:{1}:{2}", PluginType, PluginSubtype, Type); }
    }
}
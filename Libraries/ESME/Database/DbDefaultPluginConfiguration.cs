using System.ComponentModel.DataAnnotations;
using ESME.Plugins;

namespace ESME.Database
{
    [ComplexType]
    public class DbDefaultPluginConfiguration
    {
        public DbDefaultPluginConfiguration(DefaultPluginConfiguration defaultPluginConfiguration) { _defaultPluginConfiguration = new DefaultPluginConfiguration(defaultPluginConfiguration); }

        public static implicit operator DbDefaultPluginConfiguration(DefaultPluginConfiguration defaultPluginConfiguration) { return new DbDefaultPluginConfiguration(defaultPluginConfiguration); }
        public static implicit operator DefaultPluginConfiguration(DbDefaultPluginConfiguration dbDefaultPluginConfiguration) { return new DefaultPluginConfiguration(dbDefaultPluginConfiguration._defaultPluginConfiguration); }

        readonly DefaultPluginConfiguration _defaultPluginConfiguration = new DefaultPluginConfiguration();

        public PluginType PluginType
        {
            get { return _defaultPluginConfiguration.PluginType; }
            set { _defaultPluginConfiguration.PluginType = value; }
        }
        public PluginSubtype PluginSubtype
        {
            get { return _defaultPluginConfiguration.PluginSubtype; }
            set { _defaultPluginConfiguration.PluginSubtype = value; }
        }
        public string Type
        {
            get { return _defaultPluginConfiguration.Type; }
            set { _defaultPluginConfiguration.Type = value; }
        }
    }
}
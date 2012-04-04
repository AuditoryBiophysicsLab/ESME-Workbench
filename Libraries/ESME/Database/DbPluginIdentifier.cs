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

        public DbPluginType PluginType { get; set; }
        public DbPluginSubtype PluginSubtype { get; set; }
        public string Type { get; set; }

        public override string ToString() { return string.Format("{0}:{1}:{2}", PluginType, PluginSubtype, Type); }
    }

    [ComplexType]
    public class DbPluginType
    {
        public DbPluginType() { }
        public DbPluginType(PluginType pluginType) { PluginTypeAsByte = (byte)pluginType; }
        public static implicit operator DbPluginType(PluginType pluginType) { return new DbPluginType(pluginType); }
        public static implicit operator PluginType(DbPluginType dbPluginType) { return (PluginType)dbPluginType.PluginTypeAsByte; }
        public byte PluginTypeAsByte { get; set; }
    }

    [ComplexType]
    public class DbPluginSubtype
    {
        public DbPluginSubtype() { }
        public DbPluginSubtype(PluginSubtype pluginSubtype) { PluginSubtypeAsByte = (byte)pluginSubtype; }
        public static implicit operator DbPluginSubtype(PluginSubtype pluginSubtype) { return new DbPluginSubtype(pluginSubtype); }
        public static implicit operator PluginSubtype(DbPluginSubtype dbPluginSubtype) { return (PluginSubtype)dbPluginSubtype.PluginSubtypeAsByte; }
        public byte PluginSubtypeAsByte { get; set; }
    }
}
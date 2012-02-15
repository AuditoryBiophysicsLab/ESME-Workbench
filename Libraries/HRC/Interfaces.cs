using System.IO;
using System.Windows.Controls;

namespace HRC
{
    public interface ISerializeDeserialize
    {
        void Serialize(BinaryWriter writer);
        void Deserialize(BinaryReader reader);
    }

    public interface IHRCPlugin
    {
        string PluginName { get; }
        string PluginDescription { get; }
        Control ConfigurationControl { get; }
        bool IsAvailable { get; }
    }
}

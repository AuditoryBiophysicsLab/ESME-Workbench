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
        /// <summary>
        /// Name of the plugin, intended to be displayed to the user
        /// </summary>
        string PluginName { get; }
        /// <summary>
        /// Description of the plugin, intended to be displayed to the user
        /// </summary>
        string PluginDescription { get; }
        /// <summary>
        /// A reference to a WPF Control that will handle configuration of the plugin, or null if this plugin does not require or support configuration
        /// </summary>
        Control ConfigurationControl { get; }
        /// <summary>
        /// True if this plugin is configurable, false otherwise
        /// </summary>
        bool IsConfigurable { get; }
        /// <summary>
        /// True if this plugin is selectable, false otherwise
        /// </summary>
        bool IsSelectable { get; }
        /// <summary>
        /// True if this plugin is configured, false otherwise
        /// </summary>
        bool IsConfigured { get; }
        /// <summary>
        /// The path to the DLL containing this plugin
        /// </summary>
        string DLLPath { get; set; }
    }
}

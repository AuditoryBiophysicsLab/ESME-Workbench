using System.Drawing;
using ESME.Plugins;
using HRC;

namespace ESME
{
    public interface ISelfDraw
    {
        bool Display { get; set; }
        void Draw(Graphics g);
    }

    public interface IOpenExperimentDialog
    {
        string ExperimentName { get; }
        bool? ShowDialog();
    }

    public interface IHaveALog
    {
        string Log { get; set; }
    }

    public interface IHaveAName
    {
        string Name { get; set; }
    }

    public interface IMightBeDirty
    {
        bool IsDirty { get; set; }
    }

    public interface IHaveBasePath
    {
        string BasePath { get; set; }
    }

    public interface IHaveProperties
    {
        string PropertyViewName { get; }
    }

    public interface ICanSave
    {
        void Save(string fileName);
    }

    public interface ICanLoad<out T>
    {
        T Load(string fileName);
    }

    public interface IMouseOverAware
    {
        bool IsMouseOver { get; set; }
    }

    public interface IEnvironmentFile<out T> : ICanSave, ICanLoad<T>
    { }

    public interface IESMEPlugin : IHRCPlugin
    {
        PluginType PluginType { get; }
        PluginSubtype PluginSubtype { get; }
        PluginIdentifier PluginIdentifier { get; }
        void LoadSettings();
    }
}
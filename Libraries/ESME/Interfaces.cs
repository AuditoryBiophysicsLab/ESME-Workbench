using System.Drawing;

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
}
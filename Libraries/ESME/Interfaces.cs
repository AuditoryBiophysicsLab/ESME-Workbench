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

    public interface IHasLog
    {
        string Log { get; set; }
    }

    public interface IHasName
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
}
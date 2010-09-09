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
}
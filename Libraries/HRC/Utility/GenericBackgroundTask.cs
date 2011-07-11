using System.ComponentModel;
using System.Threading;

namespace HRC.Utility
{
    public class GenericBackgroundTask : BackgroundTask
    {
        public override void Start()
        {
            RunState = "Waiting";
            RunWorkerCompleted += (s, e) => { IsDone = true; };
            RunState = "Starting";
            RunWorkerAsync(this);
        }

        protected override void Run(object sender, DoWorkEventArgs e)
        {
        }
    }
}

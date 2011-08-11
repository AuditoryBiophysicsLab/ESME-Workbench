using System;
using Microsoft.VisualBasic.ApplicationServices;

namespace TransmissionLossCalculator
{
    public class SingleInstanceApplication
    {
        [STAThread]
        public static void Main(string[] args)
        {
            var manager = new SingleInstanceManager();
            manager.Run(args);
        }
    }
    
    // Using VB bits to detect single instances and process accordingly:
    //  * OnStartup is fired when the first instance loads
    //  * OnStartupNextInstance is fired when the application is re-run again
    //    NOTE: it is redirected to this instance thanks to IsSingleInstance
    public class SingleInstanceManager : WindowsFormsApplicationBase
    {
        App _app;

        public SingleInstanceManager()
        {
            IsSingleInstance = true;
        }

        protected override bool OnStartup(StartupEventArgs e)
        {
            // First time app is launched
            _app = new App();
            _app.Run();
            return false;
        }
    }
}

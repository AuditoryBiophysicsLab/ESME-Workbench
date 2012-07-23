using System.Diagnostics;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Threading;
using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;

namespace WixBootstrapper
{
    public class Bootstrapper : BootstrapperApplication
    {
        /// <summary>
        /// Gets the global model.
        /// </summary>
        static public Model Model { get; private set; }

        /// <summary>
        /// Gets the global view.
        /// </summary>
        static public RootView View { get; private set; }
        // TODO: We should refactor things so we dont have a global View.

        /// <summary>
        /// Gets the global dispatcher.
        /// </summary>
        static public Dispatcher Dispatcher { get; private set; }

        /// <summary>
        /// Launches the default web browser to the provided URI.
        /// </summary>
        /// <param name="uri">URI to open the web browser.</param>
        public static void LaunchUrl(string uri)
        {
            // Switch the wait cursor since shellexec can take a second or so.
            var cursor = View.Cursor;
            View.Cursor = Cursors.Wait;

            try
            {
                var process = new Process {StartInfo = {FileName = uri, UseShellExecute = true, Verb = "open"}};

                process.Start();
            }
            finally
            {
                View.Cursor = cursor; // back to the original cursor.
            }
        }

        /// <summary>
        /// Thread entry point for WiX Toolset UX.
        /// </summary>
        protected override void Run()
        {
            Engine.Log(LogLevel.Verbose, "Running the ESME Bootstrapper.");
            Model = new Model(this);
            Dispatcher = Dispatcher.CurrentDispatcher;
            var backgroundColorString = Engine.StringVariables["BackgroundColor"];
            var progressBarColorString = Engine.StringVariables["ProgressBarColor"];
            Engine.Log(LogLevel.Verbose, string.Format("BackgroundColor read as \"{0}\"", backgroundColorString));
            var convertedBackgroundColorObject = ColorConverter.ConvertFromString(backgroundColorString);
            var convertedProgressBarColorObject = ColorConverter.ConvertFromString(progressBarColorString);
            var backgroundColor = Colors.LightSeaGreen;
            var progressBarColor = Color.FromArgb(0xFF, 0x00, 0x8E, 0x91);
            if (convertedBackgroundColorObject == null) Engine.Log(LogLevel.Verbose, string.Format("ColorConverter.ConvertFromString({0}) returned NULL", backgroundColorString));
            else
            {
                backgroundColor = (Color)convertedBackgroundColorObject;
                Engine.Log(LogLevel.Verbose, string.Format("ColorConverter.ConvertFromString({0}) returned A:{1} R:{2} G:{3} B:{4}", backgroundColorString, backgroundColor.A, backgroundColor.R, backgroundColor.G, backgroundColor.B));
            }
            if (convertedProgressBarColorObject != null) progressBarColor = (Color)convertedProgressBarColorObject;
            //var backgroundColor = (Color)(ColorConverter.ConvertFromString(Engine.StringVariables["BackgroundColor"]) ?? Colors.LightSeaGreen);
            //var progressBarColor = (Color)(ColorConverter.ConvertFromString(Engine.StringVariables["ProgressBarColor"]) ?? ColorConverter.ConvertFromString("#FF008E91"));
            var viewModel = new RootViewModel
            {
                ProductLongName = Engine.StringVariables["ProductLongName"],
                ProductShortName = Engine.StringVariables["ProductShortName"],
                BundleLongName = Engine.StringVariables["BundleLongName"],
                ButtonBackgroundBrush = new SolidColorBrush(backgroundColor),
                ProgressBarBrush = new SolidColorBrush(progressBarColor),
            };
            // Populate the view models with the latest data. This is where Detect is called.
            viewModel.Refresh();

            // Create a Window to show UI.
            if (Model.Command.Display == Display.Passive ||
                Model.Command.Display == Display.Full)
            {
                Engine.Log(LogLevel.Verbose, "Creating a UI.");
                View = new RootView(viewModel);
                View.Show();
            }
            Dispatcher.Run();

            Engine.Quit(0);
        }
    }
}

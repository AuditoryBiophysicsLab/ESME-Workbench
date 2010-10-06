using System;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Text;
using Cinch;
using ESMEWorkBench.Data;

namespace ESMEWorkBench
{
    public static class Globals
    {
        static Globals()
        {
            AppSettings = AppSettings.Load(AppSettings.AppSettingsFile, null);
        }

        public static AppSettings AppSettings { get; set; }

        public static void DisplayException(IMessageBoxService messageBoxService, Exception ex, string format, params object[] args)
        {
            var sb = new StringBuilder(string.Format(format, args));
            sb.Append("\n\n");
            while (ex != null)
            {
                if (ex is CompositionException)
                {
                    var compositionException = (CompositionException) ex;
                    foreach (var error in compositionException.Errors)
                    {
                        sb.Append(error.Description + "\n");
                    }
                }
                else
                {
                    sb.Append(ex.Message + "\n");
                }
                ex = ex.InnerException;
            }
            messageBoxService.ShowError(sb.ToString());
            Debug.WriteLine(sb.ToString());
            using (var sw = new StreamWriter(App.Logfile, true))
                sw.WriteLine(sb.ToString());
        }
    }
}

using System;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using Cinch;
using ESME.Data;

namespace ESMEWorkBench
{
    public static class Globals
    {
        static Globals()
        {
            AppSettings.ApplicationName = App.Name;
            AppSettings = AppSettings.Load(AppSettings.AppSettingsFile, null);
            AppSettings.CASSSettings.SetDefaultCASSParameterFiles();
        }

        public static AppSettings AppSettings { get; set; }

        public static void DisplayException(IMessageBoxService messageBoxService, Exception ex, string format, params object[] args)
        {
            Trace.WriteLine("ESME WorkBench encountered an exception: " + ex.Message);
            Trace.WriteLine("Stack trace:");
            Trace.WriteLine(ex.StackTrace);
            var sb = new StringBuilder(string.Format(format, args));
            sb.Append("\n");
            while (ex != null)
            {
                if (ex is CompositionException)
                {
                    var compositionException = (CompositionException) ex;
                    foreach (var error in compositionException.Errors)
                    {
                        sb.Append(ex.GetType() + ": " + error.Description + "\n");
                    }
                }
                else
                {
                    sb.Append(ex.GetType() + ": " + ex.Message + "\n");
                }
                ex = ex.InnerException;
            }
            messageBoxService.ShowError(sb.ToString());
        }

        public static string Filter(this string s, Func<char, bool> trueIfKeep)
        {
            if (!string.IsNullOrEmpty(s))
            {
                var sb = new StringBuilder(s.Length);
                foreach (var c in s.Where(c => trueIfKeep(c)))
                    sb.Append(c);

                return sb.ToString();
            }
            return s;
        }
    }
}

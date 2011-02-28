using System;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using Cinch;
using ESME.Data;
using ESMEWorkBench.Data;

namespace ESMEWorkBench
{
    public static class Globals
    {
        static Globals()
        {
            AppSettings.ApplicationName = "ESME WorkBench";
            AppSettings = AppSettings.Load(AppSettings.AppSettingsFile, null);
        }

        public static AppSettings AppSettings { get; set; }

        public static void DisplayException(IMessageBoxService messageBoxService, Exception ex, string format, params object[] args)
        {
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
#if DEBUG
            Debug.WriteLine(sb.ToString());
            using (var sw = new StreamWriter(App.Logfile, true))
                sw.WriteLine(sb.ToString());
#endif
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

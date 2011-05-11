using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using Cinch;
using ESME.Data;
using ESME.TransmissionLoss;

namespace ESMEWorkBench
{
    public static class Globals
    {
        static Globals()
        {
            AppSettings.ApplicationName = App.Name;
            AppSettings = AppSettings.Load(AppSettings.AppSettingsFile);
            AppSettings.SetDefaults();
        }

        public static AppSettings AppSettings { get; set; }

        public static void DisplayException(IMessageBoxService messageBoxService, Exception ex, string format, params object[] args)
        {
            var originalException = ex;
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
            Trace.WriteLine("Exception encountered: " + sb);
            Trace.WriteLine("Exception stack trace:");
            Trace.WriteLine(originalException.StackTrace);
            if (originalException.InnerException != null)
            {
                Trace.WriteLine("Inner exception message: " + originalException.InnerException.Message);
                Trace.WriteLine("Inner exception stack trace:");
                Trace.WriteLine(originalException.InnerException.StackTrace);
            }
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

        public static List<TransmissionLossAlgorithm> ValidTransmissionLossAlgorithms = new List<TransmissionLossAlgorithm>
                                                                                        {
                                                                                            TransmissionLossAlgorithm.CASS,
                                                                                            TransmissionLossAlgorithm.RAM,
                                                                                            TransmissionLossAlgorithm.Bellhop,
                                                                                            TransmissionLossAlgorithm.RAMGEO
                                                                                        };
    }
}

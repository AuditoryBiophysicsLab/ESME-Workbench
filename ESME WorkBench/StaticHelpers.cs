using System;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Text;

namespace ESMEWorkbench
{
    public static class StaticHelpers
    {
        public static void DisplayException(Exception ex, string format, params object[] args)
        {
            var originalException = ex;
            var sb = new StringBuilder(string.Format(format, args));
            sb.Append("\n");
            while (ex != null)
            {
                if (ex is CompositionException)
                {
                    var compositionException = (CompositionException)ex;
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
            ESME.Globals.MessageBoxService.ShowError(sb.ToString());
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
    }
}

using System;
using System.Text;
using Cinch;
using ESMEWorkBench.Data;

namespace ESMEWorkBench
{
    public static class Globals
    {
        static Globals()
        {
            AppSettings = AppSettings.Load(AppSettings.AppSettingsFile);
            //EnvironmentDatabaseViewModel = new EnvironmentDatabaseViewModel(AppSettings.EnvironmentDatabaseDirectory);
        }

        public static AppSettings AppSettings { get; set; }
        public static bool IsInitializeExperimentNeeded { get; set; }

        public static void DisplayException(IMessageBoxService messageBoxService, Exception ex, string format, params object[] args)
        {
            var sb = new StringBuilder(string.Format(format, args));
            sb.Append("\n\n");
            sb.Append(ex.Message);
            while (ex.InnerException != null)
            {
                sb.Append(":\n" + ex.InnerException);
                ex = ex.InnerException;
            }
            messageBoxService.ShowError(sb.ToString());
        }
    }
}

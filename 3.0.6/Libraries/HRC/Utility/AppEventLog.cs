using System;
using System.Diagnostics;
using System.Security;

namespace HRC.Utility
{
    public class AppEventLog
    {
        public AppEventLog(string appName) { AppName = appName; }
        public string AppName { get; set; }
        public string DebugAppName { get; set; }

        public void Write(EventLogEntryType eventLogEntryType, string formatString, params object[] args)
        {
            if (AppName == null) throw new ApplicationException("AppEventLog: AppName must be set before calling Write()");

            try
            {
                if (!EventLog.SourceExists(AppName)) EventLog.CreateEventSource(AppName, "Application");

                EventLog.WriteEntry(AppName, string.Format(formatString, args), eventLogEntryType);
            }
            catch (SecurityException) {}
        }

        public void Debug(EventLogEntryType eventLogEntryType, string formatString, params object[] args)
        {
#if DEBUG
            if (AppName == null) throw new ApplicationException("AppEventLog: AppName must be set before calling Debug()");
            if (DebugAppName == null) DebugAppName = AppName + "_Debug";

            try
            {
                if (!EventLog.SourceExists(DebugAppName)) EventLog.CreateEventSource(DebugAppName, "Application");

                EventLog.WriteEntry(DebugAppName, string.Format(formatString, args), eventLogEntryType);
            }
            catch (SecurityException) {}
#endif
        }
    }
}
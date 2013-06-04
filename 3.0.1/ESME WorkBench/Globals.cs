﻿using System;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Text;
using ESME.Data;
using HRC.Services;

namespace ESMEWorkbench
{
    public static class Globals
    {
        static Globals()
        {
            AppSettings.ApplicationName = App.Name;
            AppSettings = AppSettings.Load(AppSettings.AppSettingsFile);
            AppSettings.Save();
            ESME.Globals.AppSettings = AppSettings;
        }

        static AppSettings _appSettings;
        public static AppSettings AppSettings
        {
            get { return _appSettings; }
            set
            {
                if (_appSettings == value) return;
                _appSettings = value;
                ESME.Globals.AppSettings = value;
                HRCOpenFileService.InitialDirectories = _appSettings.OpenFileServiceDirectories;
            }
        }

        public static void DisplayException(IMessageBoxService messageBoxService, Exception ex, string format, params object[] args)
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
    }
}
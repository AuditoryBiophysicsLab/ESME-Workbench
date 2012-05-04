using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ESME.Data;
using ESME.TransmissionLoss;
using HRC.Services;

namespace ESME
{
    public static class Globals
    {
        static Globals()
        {
        }

        public static string Filter(this string s, Func<char, bool> trueIfKeep)
        {
            if (!string.IsNullOrEmpty(s))
            {
                var sb = new StringBuilder(s.Length);
                foreach (var c in s.Where(trueIfKeep)) sb.Append(c);

                return sb.ToString();
            }
            return s;
        }

        static AppSettings _appSettings;
        public static AppSettings AppSettings
        {
            get { return _appSettings; }
            set
            {
                if (_appSettings == value) return;
                _appSettings = value;
                HRCOpenFileService.InitialDirectories = _appSettings.OpenFileServiceDirectories;
            }
        }

        public static WorkDirectories WorkDirectories { get; set; }
    }
}
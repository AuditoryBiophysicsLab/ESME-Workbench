using ESME.Data;
using HRC.Services;

namespace ESME
{
    public static class Globals
    {
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
    }
}
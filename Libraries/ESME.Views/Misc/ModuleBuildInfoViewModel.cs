using System;
using System.IO;
using System.Reflection;

namespace ESME.Views.Misc
{
    public class ModuleBuildInfoViewModel
    {
        public ModuleBuildInfoViewModel(string moduleName, DateTime buildDateTime, string buildEngineer)
        {
            var appDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
            VersionString = Assembly.LoadFile(Path.Combine(appDir, moduleName)).GetName().Version.ToString();
            ModuleName = moduleName;
            BuildDateTime = buildDateTime;
            BuildEngineer = buildEngineer;
        }

        public ModuleBuildInfoViewModel(string moduleName, DateTime buildDateTime, string buildEngineer, string versionString)
        {
            ModuleName = moduleName;
            BuildDateTime = buildDateTime;
            BuildEngineer = buildEngineer;
            VersionString = versionString;
        }
        public string ModuleName { get; private set; }
        public DateTime BuildDateTime { get; private set; }
        public string BuildEngineer { get; private set; }
        public string VersionString { get; private set; }
    }
}
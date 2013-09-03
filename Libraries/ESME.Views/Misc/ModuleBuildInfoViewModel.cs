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
            var version = Assembly.LoadFile(Path.Combine(appDir, moduleName)).GetName().Version;
            VersionString = String.Format("{0}.{1}.{2}.{3}", version.Major, version.Minor, version.Build, BuildInformation.GitHash.Substring(0, 8));

            ModuleName = moduleName;
            BuildDateTime = buildDateTime;
            BuildEngineer = buildEngineer;
        }

        public ModuleBuildInfoViewModel(string moduleName, DateTime buildDateTime, string buildEngineer, Version version)
        {
            ModuleName = moduleName;
            BuildDateTime = buildDateTime;
            BuildEngineer = buildEngineer;
            VersionString = String.Format("{0}.{1}.{2}.{3}", version.Major, version.Minor, version.Build, BuildInformation.GitHash.Substring(0, 8));
        }
        public string ModuleName { get; private set; }
        public DateTime BuildDateTime { get; private set; }
        public string BuildEngineer { get; private set; }
        public string VersionString { get; private set; }
    }
}
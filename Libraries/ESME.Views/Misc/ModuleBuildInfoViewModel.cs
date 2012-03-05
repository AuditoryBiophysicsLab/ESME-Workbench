using System;

namespace ESME.Views.Misc
{
    public class ModuleBuildInfoViewModel
    {
        public ModuleBuildInfoViewModel(string moduleName, DateTime buildDateTime, string buildEngineer, string svnVersion)
        {
            ModuleName = moduleName;
            BuildDateTime = buildDateTime;
            BuildEngineer = buildEngineer;
            SVNVersion = svnVersion;
        }

        public string ModuleName { get; private set; }
        public DateTime BuildDateTime { get; private set; }
        public string BuildEngineer { get; private set; }
        public string SVNVersion { get; private set; }
    }
}
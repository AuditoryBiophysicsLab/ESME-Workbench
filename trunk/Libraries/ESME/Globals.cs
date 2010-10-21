using System.IO;

namespace ESME
{
    internal static class Globals
    {
        public static string UserFolder
        {
            get { return Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "ESME WorkBench"); }
        }
    }
}
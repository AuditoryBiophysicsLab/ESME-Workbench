using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace ESME
{
    static class Globals
    {
        public static string UserFolder
        {
            get
            {
                return Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "ESME WorkBench");
            }
        }
    }
}

using System;
using System.IO;
using System.Linq;
using System.Text;
using ESME.NEMO;
using HRC.Navigation;

namespace ESME
{
    internal static class Globals
    {
        public static string UserFolder
        {
            get { return Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "ESME WorkBench"); }
        }

        public static string Filter(this string s, Func<char, bool> trueIfKeep)
        {
            if (!string.IsNullOrEmpty(s))
            {
                var sb = new StringBuilder(s.Length);
                foreach (var c in s.Where(c => trueIfKeep(c)))
                    sb.Append(c);

                return sb.ToString();
            }
            return s;
        }
    }
}
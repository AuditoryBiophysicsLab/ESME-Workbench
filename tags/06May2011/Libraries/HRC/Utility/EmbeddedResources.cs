using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;
using System.IO;

namespace HRC.Utility
{
    public class EmbeddedResources
    {
        public static string GetTextFromResource(string TextResourceName)
        {
            Assembly a = Assembly.GetCallingAssembly();
            string[] resNames = a.GetManifestResourceNames();
            string result;

            foreach (string s in resNames)
            {
                if (s.EndsWith(TextResourceName))
                {
                    using (Stream stream = a.GetManifestResourceStream(s))
                    {
                        StreamReader t = new StreamReader(stream);
                        result = t.ReadToEnd();
                        t.Close();
                    }
                    return result;
                }
            }
            return null;
        }
    }
}

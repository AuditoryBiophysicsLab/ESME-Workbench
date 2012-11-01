using System.Linq;
using System.Reflection;
using System.IO;

namespace HRC.Utility
{
    public class EmbeddedResources
    {
        public static string GetTextFromResource(string textResourceName)
        {
            var a = Assembly.GetCallingAssembly();
            var resNames = a.GetManifestResourceNames();

            foreach (var s in resNames.Where(s => s.EndsWith(textResourceName))) 
                using (var stream = a.GetManifestResourceStream(s)) 
                    if (stream != null) 
                        using (var t = new StreamReader(stream)) return t.ReadToEnd();
            return null;
        }
    }
}

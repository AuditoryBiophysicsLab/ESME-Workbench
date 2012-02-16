using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;

namespace HRC.Plugins
{
    public static class PluginManager
    {
        public static Dictionary<Type, T> FindPlugins<T>(string folder, Func<T, bool> filter = null) where T: IHRCPlugin
        {
            Debug.Assert(typeof(T).IsInterface);

            var files = Directory.GetFiles(folder, "*.dll");
            var result = new Dictionary<Type, T>();
            foreach (var file in files)
            {
                try
                {
                    var assembly = Assembly.Load(File.ReadAllBytes(file));
                    var matchingTypes = from type in assembly.GetTypes()
                                        where type.IsClass && !type.IsNotPublic
                                        let interfaces = type.GetInterfaces()
                                        where interfaces.Contains(typeof (T))
                                        select type;
                    foreach (var curType in matchingTypes)
                    {
                        var instance = (T)Activator.CreateInstance(curType);
                        instance.DLLPath = file;
                        if ((filter == null) || filter(instance)) result.Add(curType, instance);
                    }
                }
                catch (Exception ex)
                {
                    Debug.WriteLine("{0}: PluginManager.FindPlugins({1}) caught exception in file: {2}", DateTime.Now, folder, file);
                    Debug.WriteLine("{0}:     Exception: {1}", DateTime.Now, ex.Message);
                }
            }

            return result;
        }
    }
}

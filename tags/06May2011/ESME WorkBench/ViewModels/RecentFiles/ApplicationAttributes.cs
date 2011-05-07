using System;
using System.Reflection;

namespace ESMEWorkBench.ViewModels.RecentFiles
{
    static class ApplicationAttributes
    {
        static ApplicationAttributes()
        {
            Assembly assembly;

            Title = String.Empty;
            CompanyName = String.Empty;
            Copyright = String.Empty;
            ProductName = String.Empty;
            Version = String.Empty;

            try
            {
                assembly = Assembly.GetEntryAssembly();

                if (assembly != null)
                {
                    var attributes = assembly.GetCustomAttributes(false);

                    foreach (var attribute in attributes)
                    {
                        var type = attribute.GetType();

                        if (type == typeof(AssemblyTitleAttribute)) Title = ((AssemblyTitleAttribute) attribute).Title;
                        if (type == typeof(AssemblyCompanyAttribute)) CompanyName = ((AssemblyCompanyAttribute) attribute).Company;
                        if (type == typeof(AssemblyCopyrightAttribute)) Copyright = ((AssemblyCopyrightAttribute) attribute).Copyright;
                        if (type == typeof(AssemblyProductAttribute)) ProductName = ((AssemblyProductAttribute) attribute).Product;
                    }

                    if (assembly.GetName().Version != null)
                        Version = assembly.GetName().Version.ToString();
                }
            }
// ReSharper disable EmptyGeneralCatchClause
            catch (Exception) {}
// ReSharper restore EmptyGeneralCatchClause
        }

        public static string Title { get; private set; }
        public static string CompanyName { get; private set; }
        public static string Copyright { get; private set; }
        public static string ProductName { get; private set; }

        public static string Version { get; private set; }
    }
}
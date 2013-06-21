using System;
using System.Collections.Generic;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Text;

namespace HRC.Utility
{
    public static class MsiUtilityFunctions
    {
        [DllImport("msi.dll", CharSet = CharSet.Unicode)]
        static extern Int32 MsiGetProductInfo(string product, string property, [Out] StringBuilder valueBuf, ref Int32 len);
        public static DateTime GetInstalledDate(string productGuid)
        {
            var len = 512;
            var builder = new StringBuilder(len);
            MsiGetProductInfo(productGuid, "InstallDate", builder, ref len);
            return DateTime.ParseExact(builder.ToString(), "yyyyMMdd", CultureInfo.InvariantCulture);
        }

        public static string GetInstalledVersion(string productGuid, out int?[] versionFields)
        {
            var len = 512;
            var builder = new StringBuilder(len);
            MsiGetProductInfo(productGuid, "ProductVersion", builder, ref len);
            var result = new List<int?>();
            var fields = builder.ToString().Split('.');
            foreach (var field in fields)
            {
                if (string.IsNullOrEmpty(field)) result.Add(null);
                else
                {
                    int fieldValue;
                    result.Add(int.TryParse(field, out fieldValue) ? fieldValue : (int?)null);
                }
            }
            versionFields = result.ToArray();
            return builder.ToString();
        }
    }
}

using System;
using System.Collections.Generic;
using Microsoft.Win32;

namespace OneNavyModel.ViewModels.RecentFiles
{
    class RegistryPersister : IPersist
    {
        public RegistryPersister()
        {
            RegistryKey = "Software\\" + ApplicationAttributes.CompanyName + "\\" + ApplicationAttributes.ProductName + "\\" + "RecentFileList";
        }

        public RegistryPersister(string key)
        {
            RegistryKey = key;
        }

        string RegistryKey { get; set; }

        #region IPersist Members

        public List<string> RecentFiles(int max)
        {
            var k = Registry.CurrentUser.OpenSubKey(RegistryKey) ?? Registry.CurrentUser.CreateSubKey(RegistryKey);

            var list = new List<string>(max);

            for (var i = 0; i < max; i++)
            {
                if (k == null) continue;
                var filename = (string) k.GetValue(Key(i));

                if (String.IsNullOrEmpty(filename)) break;

                list.Add(filename);
            }

            return list;
        }

        public void InsertFile(string filepath, int max)
        {
            var k = Registry.CurrentUser.OpenSubKey(RegistryKey);
            if (k == null) Registry.CurrentUser.CreateSubKey(RegistryKey);
            k = Registry.CurrentUser.OpenSubKey(RegistryKey, true);

            RemoveFile(filepath, max);

            for (var i = max - 2; i >= 0; i--)
            {
                var sThis = Key(i);
                var sNext = Key(i + 1);

                if (k == null) continue;

                var oThis = k.GetValue(sThis);
                if (oThis == null) continue;

                k.SetValue(sNext, oThis);
            }

            if (k != null) k.SetValue(Key(0), filepath);
        }

        public void RemoveFile(string filepath, int max)
        {
            var k = Registry.CurrentUser.OpenSubKey(RegistryKey);
            if (k == null) return;

            for (var i = 0; i < max; i++)
            {
                again:
                var s = (string) k.GetValue(Key(i));
                if (s == null || !s.Equals(filepath, StringComparison.CurrentCultureIgnoreCase)) continue;
                RemoveFile(i, max);
                goto again;
            }
        }

        #endregion

        static string Key(int i)
        {
            return i.ToString("00");
        }

        void RemoveFile(int index, int max)
        {
            var k = Registry.CurrentUser.OpenSubKey(RegistryKey, true);
            if (k == null) return;

            k.DeleteValue(Key(index), false);

            for (var i = index; i < max - 1; i++)
            {
                var sThis = Key(i);
                var sNext = Key(i + 1);

                var oNext = k.GetValue(sNext);
                if (oNext == null) break;

                k.SetValue(sThis, oNext);
                k.DeleteValue(sNext);
            }
        }
    }
}
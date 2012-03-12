using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.IO;
using HRC.Utility;
using System.Linq;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Settings
{
    public interface ISettingsService {
        string SettingsRootDirectory { get; set; }
        ISettingsBase this[Type settingType, string fileName] { get; set; }
    }

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof (ISettingsService))]
    //[NotifyPropertyChanged]
    public class SettingsService : ISettingsService
    {
        public string SettingsRootDirectory { get; set; }

        public ISettingsBase this[Type settingType, string fileName]
        {
            get
            {
                if (string.IsNullOrEmpty(SettingsRootDirectory) || !Directory.Exists(SettingsRootDirectory)) return null;
                if (!_typeDictionary.ContainsKey(settingType))
                {
                    var typeDirectory = (from directory in Directory.EnumerateDirectories(SettingsRootDirectory)
                                         where Path.GetFileName(directory) == settingType.ToString()
                                         select directory).FirstOrDefault();
                    if (typeDirectory == null) return null;
                    _typeDictionary.AddType(this, settingType);
                }
                if (!_typeDictionary[settingType].ContainsKey(fileName)) return null;
                return _typeDictionary[settingType][fileName] ?? (_typeDictionary[settingType][fileName] = (ISettingsBase)StaticXmlSerializer.Load(fileName, settingType));
            }
            set
            {
                if (string.IsNullOrEmpty(SettingsRootDirectory)) throw new ApplicationException("SettingsRootDirectory has not been set");
                var typeDirectory = Path.Combine(SettingsRootDirectory, settingType.ToString());
                if (!Directory.Exists(typeDirectory)) Directory.CreateDirectory(typeDirectory);
                if (!_typeDictionary.ContainsKey(settingType))
                    _typeDictionary.Add(settingType, new SettingsDictionary());
                _typeDictionary[settingType][fileName] = value;
                value.Save(Path.Combine(typeDirectory, fileName));
            }
        }

        readonly TypeDictionary _typeDictionary = new TypeDictionary();
    }

    class TypeDictionary : Dictionary<Type, SettingsDictionary>
    {
        public void AddType(SettingsService settingsService, Type type)
        {
            if (ContainsKey(type)) return;
            var typeDirectory = Path.Combine(settingsService.SettingsRootDirectory, type.ToString());
            if (!Directory.Exists(typeDirectory)) Directory.CreateDirectory(typeDirectory);
            Add(type, new SettingsDictionary(settingsService, type));
        }
    }
    class SettingsDictionary : Dictionary<string, ISettingsBase>
    {
        public SettingsDictionary() { }

        public SettingsDictionary(SettingsService settingsService, Type type)
        {
            var files = Directory.GetFiles(Path.Combine(settingsService.SettingsRootDirectory, type.ToString()), "*.*", SearchOption.TopDirectoryOnly);
            foreach (var file in files)
            {
                var localFileName = Path.GetFileName(file);
                if (localFileName != null) Add(localFileName, null);
            }
        }
    }
}

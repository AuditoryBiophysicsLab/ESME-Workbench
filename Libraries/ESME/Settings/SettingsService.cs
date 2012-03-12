using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.IO;
using System.Reflection;
using Cinch;
using HRC.Collections;
using HRC.Utility;
using MEFedMVVM.ViewModelLocator;
using System.Linq;

namespace ESME.Settings
{
    public interface ISettingsService
    {
        string SettingsRootDirectory { get; set; }
        ISettingsBase this[Type settingType, string fileName] { get; set; }
    }

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof (ISettingsService))]
    public class SettingsService : ViewModelBase, ISettingsService
    {
        #region public string SettingsRootDirectory { get; set; }

        public string SettingsRootDirectory
        {
            get { return _settingsRootDirectory; }
            set
            {
                if (_settingsRootDirectory == value) return;
                _settingsRootDirectory = value;
                NotifyPropertyChanged(SettingsRootDirectoryChangedEventArgs);
            }
        }

        public ISettingsBase this[Type settingType, string fileName]
        {
            get
            {
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
                if (!_typeDictionary.ContainsKey(settingType))
                    _typeDictionary.Add(settingType, new SettingsDictionary());
                _typeDictionary[settingType][fileName] = value;
            }
        }

        static readonly PropertyChangedEventArgs SettingsRootDirectoryChangedEventArgs = ObservableHelper.CreateArgs<SettingsService>(x => x.SettingsRootDirectory);
        string _settingsRootDirectory;

        #endregion

        readonly TypeDictionary _typeDictionary = new TypeDictionary();

        class TypeDictionary : ObservableConcurrentDictionary<Type, SettingsDictionary>
        {
            public void AddType(ISettingsService settingsService, Type type)
            {
                if (ContainsKey(type)) return;
                var typeDirectory = Path.Combine(settingsService.SettingsRootDirectory, type.ToString());
                if (!Directory.Exists(typeDirectory)) Directory.CreateDirectory(typeDirectory);
                Add(type, new SettingsDictionary(settingsService, type));
            }
        }
        class SettingsDictionary : ObservableConcurrentDictionary<string, ISettingsBase>
        {
            public SettingsDictionary() {}

            public SettingsDictionary(ISettingsService settingsService, Type type)
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
}

using System;
using System.Collections.Generic;
using System.IO;
using System.Xml.Serialization;
using ESME.Environment.NAVO;
using ESME.Plugins;
using HRC.Aspects;
using HRC.Collections;
using HRC.Utility;
using HRC.Validation;

namespace ESME.Data
{
    [Serializable]
    public class AppSettings : ValidatingViewModel
    {
        public static readonly List<Type> ReferencedTypes = new List<Type>
        {
            typeof (NAVOConfiguration),
        };

        static string _appSettingsDirectory;
        
        public static string ApplicationName
        {
            get { return _appName; }
            set
            {
                _appName = value;
                _appSettingsDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), _appName);
                if (!Directory.Exists(_appSettingsDirectory)) Directory.CreateDirectory(_appSettingsDirectory);
                AppSettingsFile = Path.Combine(_appSettingsDirectory, "settings.xml");
            }
        }

        static string _appName;

        public static string AppSettingsFile { get; private set; }

        public void Save()
        {
            StaticXmlSerializer.Save(AppSettingsFile, this);
        }

        public static AppSettings Load()
        {
            return (AppSettings)StaticXmlSerializer.Load(AppSettingsFile, typeof(AppSettings));
        }

        public static AppSettings Load(string fileName)
        {
            return (AppSettings)StaticXmlSerializer.Load(fileName, typeof(AppSettings));
        }

        [Initialize]
        public SerializableDictionary<string, string> OpenFileServiceDirectories { get; set; }

        [Initialize]
        public BellhopSettings BellhopSettings { get; set; }

        [Initialize]
        public RAMSettings RAMSettings { get; set; }

        [Initialize]
        public NAVOConfiguration NAVOConfiguration { get; set; }

        [Initialize(true)]
        public bool DisplayContoursOnTransmissionLoss { get; set; }

        [Initialize(120f)]
        public float TransmissionLossContourThreshold { get; set; }

        [Initialize(-1)]
        public int MaxImportThreadCount { get; set; }

        #region public List<PluginIdentifier> DefaultPluginIdentifiers { get; set; }
        // DefaultPluginIdentifiers should always come from the PluginManagerService.DefaultPluginIdentifiers
        // During application startup (initial deserialization of AppSettings, specificially), PluginManagerService has not yet been set.
        // Therefore, when PluginManagerService is null, we temporarily store DefaultPluginIdentifiers in _defaultPluginIdentifiers
        // When PluginManagerService gets set, PluginManagerService.DefaultPluginIdentifiers gets initialized from _defaultPluginIdentifiers
        public List<PluginIdentifier> DefaultPluginIdentifiers
        {
            get
            {
                return PluginManagerService == null ? _defaultPluginIdentifiers : PluginManagerService.DefaultPluginIdentifiers;
            }
            set
            {
                if (PluginManagerService == null) _defaultPluginIdentifiers = value;
                else PluginManagerService.DefaultPluginIdentifiers = value;
            }
        }

        List<PluginIdentifier> _defaultPluginIdentifiers;

        IPluginManagerService _pluginManagerService;

        // This is not serialized, but rather is set when the main application starts up (for ESME Workbench, that's in the MainViewModel
        // constructor).  The collection of default plugin identifiers is cached in _defaultPluginIdentifiers
        [XmlIgnore]
        public IPluginManagerService PluginManagerService
        {
            get { return _pluginManagerService; }
            set
            {
                _pluginManagerService = value;
                if (_pluginManagerService != null) _pluginManagerService.DefaultPluginIdentifiers = _defaultPluginIdentifiers;
            }
        }
        #endregion

        public string DatabaseDirectory { get; set; }
    }

    public sealed class PluginSelection : ValidatingViewModel
    {
        public string DllFilename { get; set; }

        public string ClassName { get; set; }
    }

    public sealed class RAMSettings : ValidatingViewModel
    {
        public RAMSettings()
        {
            ValidationRules.AddRange(new List<ValidationRule>
                                         {
                                             new ValidationRule
                                                 {
                                                     PropertyName = "MaximumDepth",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((RAMSettings) o).MaximumDepth;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "DepthStepSize",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((RAMSettings) o).DepthStepSize;
                                                                            return RangeCheck(ruleTarget, 0,float.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "MaximumRange",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((RAMSettings)o).MaximumRange;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "RangeStepSize",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((RAMSettings) o).RangeStepSize;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },
                                         }
                );
        }

        [Initialize(2000f)]
        public float MaximumDepth { get; set; }

        [Initialize(25f)]
        public float DepthStepSize { get; set; }

        [Initialize(100000f)]
        public float MaximumRange { get; set; }

        [Initialize(50f)]
        public float RangeStepSize { get; set; }
    }

    public sealed class BellhopSettings : ValidatingViewModel
    {
        public BellhopSettings()
        {
            ValidationRules.AddRange(new List<ValidationRule>
                                         {
                                             new ValidationRule
                                                 {
                                                     PropertyName = "MaximumDepth",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((BellhopSettings)o).MaximumDepth;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },

                                             new ValidationRule
                                                 {
                                                     PropertyName = "RangeCellSize",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget =((BellhopSettings) o).RangeCellSize;
                                                                            return RangeCheck(ruleTarget, 0,float.MaxValue, false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "DepthCellSize",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((BellhopSettings) o).DepthCellSize;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "RayCount",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((BellhopSettings) o).RayCount;
                                                                            return RangeCheck(ruleTarget,0,int.MaxValue,false);
                                                                        },
                                                 },

                                         });
        }

        [Initialize(2000f)]
        public float MaximumDepth { get; set; }

        [Initialize(10f)]
        public float RangeCellSize { get; set; }

        [Initialize(10f)]
        public float DepthCellSize { get; set; }

        [Initialize(3000)]
        public int RayCount { get; set; }
    }
}
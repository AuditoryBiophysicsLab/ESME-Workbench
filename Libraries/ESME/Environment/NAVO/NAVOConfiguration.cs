using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using Cinch;
using HRC.Validation;

namespace ESME.Environment.NAVO
{
    [Serializable]
    public sealed class NAVOConfiguration : ValidatingViewModel
    {
        public NAVOConfiguration()
        {
            ValidationRules.AddRange(new List<ValidationRule>
            {
                    new ValidationRule
                    {
                            PropertyName = "GDEMDirectory",
                            Description = "Directory must exist and contain many uncompressed .nc files",
                            RuleDelegate = (o, r) =>
                            {
                                var ruleTarget = ((NAVOConfiguration)o).GDEMDirectory;
                                if (!Directory.Exists(ruleTarget)) return false;
                                return Directory.GetFiles(ruleTarget, "*.nc").Length > 1;
                            },
                    },
                    new ValidationRule
                    {
                            PropertyName = "SMGCDirectory",
                            Description = "Directory must exist and contain many .stt files",
                            RuleDelegate = (o, r) =>
                            {
                                var ruleTarget = ((NAVOConfiguration)o).SMGCDirectory;
                                if (!Directory.Exists(ruleTarget)) return false;
                                return
                                        (File.Exists(Path.Combine(ruleTarget, "n00e009.stt")) &&
                                         File.Exists(Path.Combine(ruleTarget, "n52e117.stt")) &&
                                         File.Exists(Path.Combine(ruleTarget, "n88e091.stt")));
                            },
                    },
                    new ValidationRule
                    {
                            PropertyName = "BSTDirectory",
                            Description = "File must exist.",
                            RuleDelegate = (o, r) =>
                            {
                                var ruleTarget = ((NAVOConfiguration)o).BSTDirectory;
                                return File.Exists(ruleTarget);
                            },
                    },
                    new ValidationRule
                    {
                            PropertyName = "DBDBDirectory",
                            Description = "File must exist.",
                            RuleDelegate = (o, r) =>
                            {
                                var ruleTarget = ((NAVOConfiguration)o).DBDBDirectory;
                                return File.Exists(ruleTarget);
                            },
                    },
                    new ValidationRule
                    {
                            PropertyName = "DBDBEXEPath",
                            Description = "File must exist.",
                            RuleDelegate = (o, r) =>
                            {
                                var ruleTarget = ((NAVOConfiguration)o).DBDBEXEPath;
                                return File.Exists(ruleTarget);
                            },
                    },
                    new ValidationRule
                    {
                            PropertyName = "HFBLEXEPath",
                            Description = "File must exist.",
                            RuleDelegate = (o, r) =>
                            {
                                var ruleTarget = ((NAVOConfiguration)o).HFBLEXEPath;
                                return File.Exists(ruleTarget);
                            },
                    },
                    new ValidationRule
                    {
                            PropertyName = "LFBLEXEPath",
                            Description = "File must exist.",
                            RuleDelegate = (o, r) =>
                            {
                                var ruleTarget = ((NAVOConfiguration)o).LFBLEXEPath;
                                return File.Exists(ruleTarget);
                            },
                    },
            });

        }

        public void SetDefaults()
        {
            if (string.IsNullOrEmpty(DBDBEXEPath)) DBDBEXEPath = Path.Combine(Path.GetDirectoryName(Assembly.GetCallingAssembly().Location), "dbv5_command.exe");
        }

        #region public TimePeriod SpringStartMonth { get; set; }

        public TimePeriod SpringStartMonth
        {
            get { return _springStartMonth; }
            set
            {
                if (_springStartMonth == value) return;
                _springStartMonth = value;
                NotifyPropertyChanged(SpringStartMonthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SpringStartMonthChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.SpringStartMonth);
        TimePeriod _springStartMonth = TimePeriod.March;

        #endregion

        #region public TimePeriod SummerStartMonth { get; set; }

        public TimePeriod SummerStartMonth
        {
            get { return _summerStartMonth; }
            set
            {
                if (_summerStartMonth == value) return;
                _summerStartMonth = value;
                NotifyPropertyChanged(SummerStartMonthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SummerStartMonthChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.SummerStartMonth);
        TimePeriod _summerStartMonth = TimePeriod.June;

        #endregion

        #region public TimePeriod FallStartMonth { get; set; }

        public TimePeriod FallStartMonth
        {
            get { return _fallStartMonth; }
            set
            {
                if (_fallStartMonth == value) return;
                _fallStartMonth = value;
                NotifyPropertyChanged(FallStartMonthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs FallStartMonthChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.FallStartMonth);
        TimePeriod _fallStartMonth = TimePeriod.September;

        #endregion

        #region public TimePeriod WinterStartMonth { get; set; }

        public TimePeriod WinterStartMonth
        {
            get { return _winterStartMonth; }
            set
            {
                if (_winterStartMonth == value) return;
                _winterStartMonth = value;
                NotifyPropertyChanged(WinterStartMonthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WinterStartMonthChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.WinterStartMonth);
        TimePeriod _winterStartMonth = TimePeriod.December;

        #endregion

        #region public TimePeriod ColdSeasonStartMonth { get; set; }

        public TimePeriod ColdSeasonStartMonth
        {
            get { return _coldSeasonStartMonth; }
            set
            {
                if (_coldSeasonStartMonth == value) return;
                _coldSeasonStartMonth = value;
                NotifyPropertyChanged(ColdSeasonStartMonthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ColdSeasonStartMonthChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.ColdSeasonStartMonth);
        TimePeriod _coldSeasonStartMonth = TimePeriod.December;

        #endregion

        #region public TimePeriod WarmSeasonStartMonth { get; set; }

        public TimePeriod WarmSeasonStartMonth
        {
            get { return _warmSeasonStartMonth; }
            set
            {
                if (_warmSeasonStartMonth == value) return;
                _warmSeasonStartMonth = value;
                NotifyPropertyChanged(WarmSeasonStartMonthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WarmSeasonStartMonthChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.WarmSeasonStartMonth);
        TimePeriod _warmSeasonStartMonth = TimePeriod.June;

        #endregion

        #region public string GDEMDirectory { get; set; }

        public string GDEMDirectory
        {
            get { return _gdemDirectory; }
            set
            {
                if (_gdemDirectory == value) return;
                _gdemDirectory = value;
                NotifyPropertyChanged(GDEMDirectoryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs GDEMDirectoryChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.GDEMDirectory);
        string _gdemDirectory;

        public bool ValidateGDEMDirectory(string gdemFile, IMessageBoxService messageBoxService = null)
        {
            if (string.IsNullOrEmpty(gdemFile) || !File.Exists(gdemFile))
            {
                if (messageBoxService != null) messageBoxService.ShowError(string.Format("GDEM directory must point to a valid file or directory"));
                return false;
            }
            var standardFilenames = new[]
            {
                "sgdemv3s01.nc", "sgdemv3s02.nc", "sgdemv3s03.nc", "sgdemv3s04.nc", "sgdemv3s05.nc", "sgdemv3s06.nc",
                "sgdemv3s07.nc", "sgdemv3s08.nc", "sgdemv3s09.nc", "sgdemv3s10.nc", "sgdemv3s11.nc", "sgdemv3s12.nc",
                "tgdemv3s01.nc", "tgdemv3s02.nc", "tgdemv3s03.nc", "tgdemv3s04.nc", "tgdemv3s05.nc", "tgdemv3s06.nc",
                "tgdemv3s07.nc", "tgdemv3s08.nc", "tgdemv3s09.nc", "tgdemv3s10.nc", "tgdemv3s11.nc", "tgdemv3s12.nc",
            };
            var nuwcFilenames = new[]
            {
                "jan_s.nc", "feb_s.nc", "mar_s.nc", "apr_s.nc", "may_s.nc", "jun_s.nc",
                "jul_s.nc", "aug_s.nc", "sep_s.nc", "oct_s.nc", "nov_s.nc", "dec_s.nc",
                "jan_t.nc", "feb_t.nc", "mar_t.nc", "apr_t.nc", "may_t.nc", "jun_t.nc",
                "jul_t.nc", "aug_t.nc", "sep_t.nc", "oct_t.nc", "nov_t.nc", "dec_t.nc",
            };
            var gdemDirectory = Path.GetDirectoryName(gdemFile);
            var files = Directory.GetFiles(gdemDirectory, "*.nc");
            if (files.Length < 12)
            {
                if (messageBoxService != null) messageBoxService.ShowError(string.Format("Error validating GDEM directory \"{0}\": Expected file(s) not found in this directory", gdemDirectory));
                return false;
            }
            foreach (var file in files)
            {
                var curFile = Path.GetFileName(file).ToLower();
                var foundMatch = false;
                foreach (var standardFile in standardFilenames)
                    if (curFile == standardFile)
                    {
                        foundMatch = true;
                        break;
                    }
                if (foundMatch) continue;
                foreach (var nuwcFile in nuwcFilenames)
                    if (curFile == nuwcFile)
                    {
                        foundMatch = true;
                        break;
                    }
                if (foundMatch) continue;
                if (messageBoxService != null) messageBoxService.ShowError(string.Format("Error validating GDEM directory \"{0}\": Expected file(s) not found in this directory", gdemDirectory));
                return false;
            }
            GDEMDirectory = gdemDirectory;
            return true;
        }

        #endregion

        #region public string SMGCDirectory { get; set; }

        public string SMGCDirectory
        {
            get { return _smgcDirectory; }
            set
            {
                if (_smgcDirectory == value) return;
                _smgcDirectory = value;
                NotifyPropertyChanged(SMGCDirectoryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SMGCDirectoryChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.SMGCDirectory);
        string _smgcDirectory;
        public bool ValidateSMGCDirectory(string smgcFile, IMessageBoxService messageBoxService = null)
        {
            if (string.IsNullOrEmpty(smgcFile) || !File.Exists(smgcFile))
            {
                if (messageBoxService != null) messageBoxService.ShowError(string.Format("SMGC directory must point to a valid file or directory"));
                return false;
            }
            var smgcDirectory = Path.GetDirectoryName(smgcFile);
            if (smgcDirectory.ToLower().EndsWith("north") || smgcDirectory.ToLower().EndsWith("south")) smgcDirectory = Path.GetDirectoryName(smgcDirectory);
            var files = Directory.GetFiles(smgcDirectory, "*.stt");
            if (files.Length == 64800)
            {
                SMGCDirectory = smgcDirectory;
                return true;
            }
            var count = 0;
            if (Directory.Exists(Path.Combine(smgcDirectory, "north")))
                count = Directory.GetFiles(Path.Combine(smgcDirectory, "north"), "*.stt").Length;
            if (Directory.Exists(Path.Combine(smgcDirectory, "south")))
                count += Directory.GetFiles(Path.Combine(smgcDirectory, "south"), "*.stt").Length;
            if (count == 64800)
            {
                SMGCDirectory = smgcDirectory;
                return true;
            }

            if (messageBoxService != null) messageBoxService.ShowError(string.Format("Error validating SMGC directory \"{0}\": Expected file(s) not found in this directory", smgcDirectory));
            return false;
        }

        #endregion

        #region public string DBDBDirectory { get; set; }

        public string DBDBDirectory
        {
            get { return _dBDBDirectory; }
            set
            {
                if (_dBDBDirectory == value) return;
                _dBDBDirectory = value;
                NotifyPropertyChanged(DBDBDirectoryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DBDBDirectoryChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.DBDBDirectory);
        string _dBDBDirectory;

        #endregion

        #region public string BSTDirectory { get; set; }

        public string BSTDirectory
        {
            get { return _bSTDirectory; }
            set
            {
                if (_bSTDirectory == value) return;
                _bSTDirectory = value;
                NotifyPropertyChanged(BSTDirectoryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BSTDirectoryChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.BSTDirectory);
        string _bSTDirectory;

        #endregion

        #region public string DBDBEXEPath { get; set; }

        public string DBDBEXEPath
        {
            get { return _dBDBEXEPath; }
            set
            {
                if (_dBDBEXEPath == value) return;
                _dBDBEXEPath = value;
                NotifyPropertyChanged(DBDBEXEPathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DBDBEXEPathChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.DBDBEXEPath);
        string _dBDBEXEPath;

        #endregion

        #region public string HFBLEXEPath { get; set; }

        public string HFBLEXEPath
        {
            get { return _hfblEXEPath; }
            set
            {
                if (_hfblEXEPath == value) return;
                _hfblEXEPath = value;
                NotifyPropertyChanged(HFBLEXEPathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs HFBLEXEPathChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.HFBLEXEPath);
        string _hfblEXEPath;

        #endregion

        #region public string LFBLEXEPath { get; set; }

        public string LFBLEXEPath
        {
            get { return _lfblEXEPath; }
            set
            {
                if (_lfblEXEPath == value) return;
                _lfblEXEPath = value;
                NotifyPropertyChanged(LFBLEXEPathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LFBLEXEPathChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.LFBLEXEPath);
        string _lfblEXEPath;

        #endregion

        #region public string DBDBSelectedResolution { get; set; }

        public string DBDBSelectedResolution
        {
            get { return _dBDBSelectedResolution; }
            set
            {
                if (_dBDBSelectedResolution == value) return;
                _dBDBSelectedResolution = value;
                NotifyPropertyChanged(DBDBSelectedResolutionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DBDBSelectedResolutionChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.DBDBSelectedResolution);
        string _dBDBSelectedResolution;

        #endregion

        static readonly TimePeriod[] MonthMap = new[]
        {
                (TimePeriod)0,
                TimePeriod.January,
                TimePeriod.February,
                TimePeriod.March,
                TimePeriod.April,
                TimePeriod.May,
                TimePeriod.June,
                TimePeriod.July,
                TimePeriod.August,
                TimePeriod.September,
                TimePeriod.October,
                TimePeriod.November,
                TimePeriod.December,
                TimePeriod.January,
                TimePeriod.February,
                TimePeriod.March,
                TimePeriod.April,
                TimePeriod.May,
                TimePeriod.June,
        };

        public static IEnumerable<TimePeriod> AllMonths
        {
            get
            {
                yield return TimePeriod.January;
                yield return TimePeriod.February;
                yield return TimePeriod.March;
                yield return TimePeriod.April;
                yield return TimePeriod.May;
                yield return TimePeriod.June;
                yield return TimePeriod.July;
                yield return TimePeriod.August;
                yield return TimePeriod.September;
                yield return TimePeriod.October;
                yield return TimePeriod.November;
                yield return TimePeriod.December;
            }
        }

        public static IEnumerable<TimePeriod> AllSeasons
        {
            get
            {
                yield return TimePeriod.Spring;
                yield return TimePeriod.Summer;
                yield return TimePeriod.Fall;
                yield return TimePeriod.Winter;
                yield return TimePeriod.Warm;
                yield return TimePeriod.Cold;
            }
        }

        public static IEnumerable<TimePeriod> AllTimePeriods
        {
            get
            {
                yield return TimePeriod.January;
                yield return TimePeriod.February;
                yield return TimePeriod.March;
                yield return TimePeriod.April;
                yield return TimePeriod.May;
                yield return TimePeriod.June;
                yield return TimePeriod.July;
                yield return TimePeriod.August;
                yield return TimePeriod.September;
                yield return TimePeriod.October;
                yield return TimePeriod.November;
                yield return TimePeriod.December;
                yield return TimePeriod.Spring;
                yield return TimePeriod.Summer;
                yield return TimePeriod.Fall;
                yield return TimePeriod.Winter;
                yield return TimePeriod.Warm;
                yield return TimePeriod.Cold;
            }
        }

        public IEnumerable<TimePeriod> MonthsInTimePeriod(TimePeriod timePeriod)
        {
            if (SpringStartMonth == TimePeriod.Invalid || SummerStartMonth == TimePeriod.Invalid || FallStartMonth == TimePeriod.Invalid || WinterStartMonth == TimePeriod.Invalid || WarmSeasonStartMonth == TimePeriod.Invalid || ColdSeasonStartMonth == TimePeriod.Invalid)
                throw new InvalidOperationException("NAVOConfiguration: Season configuration is invalid.  One or more start months have not been set.");
            switch (timePeriod)
            {
                case TimePeriod.January:
                case TimePeriod.February:
                case TimePeriod.March:
                case TimePeriod.April:
                case TimePeriod.May:
                case TimePeriod.June:
                case TimePeriod.July:
                case TimePeriod.August:
                case TimePeriod.September:
                case TimePeriod.October:
                case TimePeriod.November:
                case TimePeriod.December:
                    yield return timePeriod;
                    yield break;
                case TimePeriod.Spring:
                    yield return MonthMap[(int)SpringStartMonth];
                    yield return MonthMap[(int)SpringStartMonth + 1];
                    yield return MonthMap[(int)SpringStartMonth + 2];
                    yield break;
                case TimePeriod.Summer:
                    yield return MonthMap[(int)SummerStartMonth];
                    yield return MonthMap[(int)SummerStartMonth + 1];
                    yield return MonthMap[(int)SummerStartMonth + 2];
                    yield break;
                case TimePeriod.Fall:
                    yield return MonthMap[(int)FallStartMonth];
                    yield return MonthMap[(int)FallStartMonth + 1];
                    yield return MonthMap[(int)FallStartMonth + 2];
                    yield break;
                case TimePeriod.Winter:
                    yield return MonthMap[(int)WinterStartMonth];
                    yield return MonthMap[(int)WinterStartMonth + 1];
                    yield return MonthMap[(int)WinterStartMonth + 2];
                    yield break;
                case TimePeriod.Cold:
                    yield return MonthMap[(int)ColdSeasonStartMonth];
                    yield return MonthMap[(int)ColdSeasonStartMonth + 1];
                    yield return MonthMap[(int)ColdSeasonStartMonth + 2];
                    yield return MonthMap[(int)ColdSeasonStartMonth + 3];
                    yield return MonthMap[(int)ColdSeasonStartMonth + 4];
                    yield return MonthMap[(int)ColdSeasonStartMonth + 5];
                    yield break;
                case TimePeriod.Warm:
                    yield return MonthMap[(int)WarmSeasonStartMonth];
                    yield return MonthMap[(int)WarmSeasonStartMonth + 1];
                    yield return MonthMap[(int)WarmSeasonStartMonth + 2];
                    yield return MonthMap[(int)WarmSeasonStartMonth + 3];
                    yield return MonthMap[(int)WarmSeasonStartMonth + 4];
                    yield return MonthMap[(int)WarmSeasonStartMonth + 5];
                    yield break;
            }
        }
    }
}

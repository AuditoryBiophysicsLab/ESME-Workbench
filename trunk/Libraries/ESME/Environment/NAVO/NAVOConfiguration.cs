using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using Cinch;
using HRC.Utility;
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
                                                                            var ruleTarget = ((NAVOConfiguration) o).GDEMDirectory;
                                                                            if(!Directory.Exists(ruleTarget))
                                                                                return false;
                                                                            return Directory.GetFiles(ruleTarget,"*.nc").Length>1;
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "SMGCDirectory",
                                                     Description = "Directory must exist and contain many .stt files",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((NAVOConfiguration) o).SMGCDirectory;
                                                                            if (!Directory.Exists(ruleTarget))
                                                                                return false;
                                                                            return
                                                                                (File.Exists(Path.Combine(ruleTarget, "n00e009.stt")) && File.Exists(Path.Combine(ruleTarget, "n52e117.stt")) && File.Exists(Path.Combine(ruleTarget, "n88e091.stt")));
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "BSTDirectory",
                                                     Description = "File must exist.",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((NAVOConfiguration) o).BSTDirectory;
                                                                            return File.Exists(ruleTarget);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "DBDBDirectory",
                                                     Description = "File must exist.",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((NAVOConfiguration) o).DBDBDirectory;
                                                                            return File.Exists(ruleTarget);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "DBDBEXEPath",
                                                     Description = "File must exist.",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((NAVOConfiguration) o).DBDBEXEPath;
                                                                            return File.Exists(ruleTarget);
                                                                        },
                                                 },                               
                               
                                         });
      
        }
             private bool ValidateBST(string filePath)
             {
                 return false;
             }

        public void SetDefaults()
        {
            if (string.IsNullOrEmpty(DBDBEXEPath)) DBDBEXEPath = Path.Combine(Path.GetDirectoryName(Assembly.GetCallingAssembly().Location), "dbv5_command.exe");
        }

        #region public NAVOTimePeriod SpringStartMonth { get; set; }

        public NAVOTimePeriod SpringStartMonth
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
        NAVOTimePeriod _springStartMonth = NAVOTimePeriod.March;

        #endregion

        #region public NAVOTimePeriod SummerStartMonth { get; set; }

        public NAVOTimePeriod SummerStartMonth
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
        NAVOTimePeriod _summerStartMonth = NAVOTimePeriod.June;

        #endregion

        #region public NAVOTimePeriod FallStartMonth { get; set; }

        public NAVOTimePeriod FallStartMonth
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
        NAVOTimePeriod _fallStartMonth = NAVOTimePeriod.September;

        #endregion

        #region public NAVOTimePeriod WinterStartMonth { get; set; }

        public NAVOTimePeriod WinterStartMonth
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
        NAVOTimePeriod _winterStartMonth = NAVOTimePeriod.December;

        #endregion

        #region public NAVOTimePeriod ColdSeasonStartMonth { get; set; }

        public NAVOTimePeriod ColdSeasonStartMonth
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
        NAVOTimePeriod _coldSeasonStartMonth = NAVOTimePeriod.December;

        #endregion

        #region public NAVOTimePeriod WarmSeasonStartMonth { get; set; }

        public NAVOTimePeriod WarmSeasonStartMonth
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
        NAVOTimePeriod _warmSeasonStartMonth = NAVOTimePeriod.June;

        #endregion

        #region public string GDEMDirectory { get; set; }

        public string GDEMDirectory
        {
            get { return _gDEMDirectory; }
            set
            {
                if (_gDEMDirectory == value) return;
                _gDEMDirectory = value;
                NotifyPropertyChanged(GDEMDirectoryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs GDEMDirectoryChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.GDEMDirectory);
        string _gDEMDirectory;

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

        static readonly NAVOTimePeriod[] MonthMap = new[]
        {
                (NAVOTimePeriod)0,
                NAVOTimePeriod.January,
                NAVOTimePeriod.February,
                NAVOTimePeriod.March,
                NAVOTimePeriod.April,
                NAVOTimePeriod.May,
                NAVOTimePeriod.June,
                NAVOTimePeriod.July,
                NAVOTimePeriod.August,
                NAVOTimePeriod.September,
                NAVOTimePeriod.October,
                NAVOTimePeriod.November,
                NAVOTimePeriod.December,
                NAVOTimePeriod.January,
                NAVOTimePeriod.February,
                NAVOTimePeriod.March,
                NAVOTimePeriod.April,
                NAVOTimePeriod.May,
                NAVOTimePeriod.June,
        };

        public IEnumerable<NAVOTimePeriod> MonthsInTimePeriod(NAVOTimePeriod timePeriod)
        {
            switch (timePeriod)
            {
                case NAVOTimePeriod.January:
                case NAVOTimePeriod.February:
                case NAVOTimePeriod.March:
                case NAVOTimePeriod.April:
                case NAVOTimePeriod.May:
                case NAVOTimePeriod.June:
                case NAVOTimePeriod.July:
                case NAVOTimePeriod.August:
                case NAVOTimePeriod.September:
                case NAVOTimePeriod.October:
                case NAVOTimePeriod.November:
                case NAVOTimePeriod.December:
                    yield return timePeriod;
                    yield break;
                case NAVOTimePeriod.Spring:
                    yield return MonthMap[(int)SpringStartMonth];
                    yield return MonthMap[(int)SpringStartMonth + 1];
                    yield return MonthMap[(int)SpringStartMonth + 2];
                    yield break;
                case NAVOTimePeriod.Summer:
                    yield return MonthMap[(int)SummerStartMonth];
                    yield return MonthMap[(int)SummerStartMonth + 1];
                    yield return MonthMap[(int)SummerStartMonth + 2];
                    yield break;
                case NAVOTimePeriod.Fall:
                    yield return MonthMap[(int)FallStartMonth];
                    yield return MonthMap[(int)FallStartMonth + 1];
                    yield return MonthMap[(int)FallStartMonth + 2];
                    yield break;
                case NAVOTimePeriod.Winter:
                    yield return MonthMap[(int)WinterStartMonth];
                    yield return MonthMap[(int)WinterStartMonth + 1];
                    yield return MonthMap[(int)WinterStartMonth + 2];
                    yield break;
                case NAVOTimePeriod.Cold:
                    yield return MonthMap[(int)ColdSeasonStartMonth];
                    yield return MonthMap[(int)ColdSeasonStartMonth + 1];
                    yield return MonthMap[(int)ColdSeasonStartMonth + 2];
                    yield return MonthMap[(int)ColdSeasonStartMonth + 3];
                    yield return MonthMap[(int)ColdSeasonStartMonth + 4];
                    yield return MonthMap[(int)ColdSeasonStartMonth + 5];
                    yield break;
                case NAVOTimePeriod.Warm:
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

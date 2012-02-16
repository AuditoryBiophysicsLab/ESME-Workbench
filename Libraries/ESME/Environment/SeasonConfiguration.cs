using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using Cinch;
using HRC.Validation;

namespace ESME.Environment
{
    public sealed class SeasonConfiguration : ValidatingViewModel
    {
        const string SeasonRuleDescription = "Must be a value between January and December, inclusive";
        public SeasonConfiguration()
        {
            ValidationRules.AddRange(new List<ValidationRule>
            {
                new ValidationRule
                {
                    PropertyName = "SpringStartMonth",
                    Description = SeasonRuleDescription,
                    RuleDelegate = (o, r) => AllMonths.Contains(((SeasonConfiguration)o).SpringStartMonth),
                },
                new ValidationRule
                {
                    PropertyName = "SummerStartMonth",
                    Description = SeasonRuleDescription,
                    RuleDelegate = (o, r) => AllMonths.Contains(((SeasonConfiguration)o).SummerStartMonth),
                },
                new ValidationRule
                {
                    PropertyName = "FallStartMonth",
                    Description = SeasonRuleDescription,
                    RuleDelegate = (o, r) => AllMonths.Contains(((SeasonConfiguration)o).FallStartMonth),
                },
                new ValidationRule
                {
                    PropertyName = "WinterStartMonth",
                    Description = SeasonRuleDescription,
                    RuleDelegate = (o, r) => AllMonths.Contains(((SeasonConfiguration)o).WinterStartMonth),
                },
                new ValidationRule
                {
                    PropertyName = "ColdSeasonStartMonth",
                    Description = SeasonRuleDescription,
                    RuleDelegate = (o, r) => AllMonths.Contains(((SeasonConfiguration)o).ColdSeasonStartMonth),
                },
                new ValidationRule
                {
                    PropertyName = "WarmSeasonStartMonth",
                    Description = SeasonRuleDescription,
                    RuleDelegate = (o, r) => AllMonths.Contains(((SeasonConfiguration)o).WarmSeasonStartMonth),
                },
            });
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

        static readonly PropertyChangedEventArgs SpringStartMonthChangedEventArgs = ObservableHelper.CreateArgs<SeasonConfiguration>(x => x.SpringStartMonth);
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

        static readonly PropertyChangedEventArgs SummerStartMonthChangedEventArgs = ObservableHelper.CreateArgs<SeasonConfiguration>(x => x.SummerStartMonth);
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

        static readonly PropertyChangedEventArgs FallStartMonthChangedEventArgs = ObservableHelper.CreateArgs<SeasonConfiguration>(x => x.FallStartMonth);
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

        static readonly PropertyChangedEventArgs WinterStartMonthChangedEventArgs = ObservableHelper.CreateArgs<SeasonConfiguration>(x => x.WinterStartMonth);
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

        static readonly PropertyChangedEventArgs ColdSeasonStartMonthChangedEventArgs = ObservableHelper.CreateArgs<SeasonConfiguration>(x => x.ColdSeasonStartMonth);
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

        static readonly PropertyChangedEventArgs WarmSeasonStartMonthChangedEventArgs = ObservableHelper.CreateArgs<SeasonConfiguration>(x => x.WarmSeasonStartMonth);
        TimePeriod _warmSeasonStartMonth = TimePeriod.June;

        #endregion

        #region Static helper properties
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
            if (SpringStartMonth == TimePeriod.Invalid || 
                SummerStartMonth == TimePeriod.Invalid || 
                FallStartMonth == TimePeriod.Invalid || 
                WinterStartMonth == TimePeriod.Invalid ||
                WarmSeasonStartMonth == TimePeriod.Invalid || 
                ColdSeasonStartMonth == TimePeriod.Invalid) 
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
        #endregion
    }
}
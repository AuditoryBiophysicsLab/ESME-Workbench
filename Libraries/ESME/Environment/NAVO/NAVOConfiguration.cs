using System;
using System.Collections.Generic;
using HRC.Aspects;
using HRC.Validation;

namespace ESME.Environment.NAVO
{

    [Serializable]
    public sealed class NAVOConfiguration : ValidatingViewModel
    {
        [Initialize(TimePeriod.March)]     public TimePeriod SpringStartMonth { get; set; }
        [Initialize(TimePeriod.June)]      public TimePeriod SummerStartMonth { get; set; }
        [Initialize(TimePeriod.September)] public TimePeriod FallStartMonth { get; set; }
        [Initialize(TimePeriod.December)]  public TimePeriod WinterStartMonth { get; set; }
        [Initialize(TimePeriod.December)]  public TimePeriod ColdSeasonStartMonth { get; set; }
        [Initialize(TimePeriod.June)]      public TimePeriod WarmSeasonStartMonth { get; set; }

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
                TimePeriod.June
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


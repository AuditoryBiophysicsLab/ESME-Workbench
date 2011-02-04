using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using Cinch;

namespace ESME.Environment.NAVO
{
    public class NAVODataSources : ViewModelBase
    {
        internal static readonly int[] MonthMap = new[]
                                         {
                                             0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6
                                         };

        public NAVODataSources(NAVOConfiguration configurations, NAVOExtractionPacket extractionPacket)
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nNAVODataSources: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }

            ExtractionPacket = extractionPacket;
            Configuration = configurations;

            Months = new List<NAVOTimePeriod>
                     {
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
                         NAVOTimePeriod.Spring,
                         NAVOTimePeriod.Summer,
                         NAVOTimePeriod.Fall,
                         NAVOTimePeriod.Winter,
                         NAVOTimePeriod.Cold,
                         NAVOTimePeriod.Warm,
                     };

            BST = new BST
                  {
                      DatabasePath = configurations.BSTDirectory,
                      ExtractionProgramPath = configurations.BSTEXEPath,
                      TimePeriod = SelectedPeriod,
                  };
            DBDB = new DBDB
                   {
                       DatabasePath = configurations.DBDBDirectory,
                       ExtractionProgramPath = configurations.DBDBEXEPath,
                       TimePeriod = SelectedPeriod,
                   };
            GDEM = new GDEM
                   {
                       DatabasePath = configurations.GDEMDirectory,
                       ExtractionProgramPath = Path.Combine(Path.GetDirectoryName(Assembly.GetCallingAssembly().Location), "ImportNetCDF.exe"), ////todo: installer needs to put this in the right place.
                       TimePeriod = SelectedPeriod,
                       GridSpacing = 0.25f,
                   };
            SMGC = new SMGC
                   {
                       DatabasePath = configurations.SMGCDirectory,
                       ExtractionProgramPath = configurations.SMGCEXEPath,
                       TimePeriod = SelectedPeriod,
                       GridSpacing = 1,
                   };

            BST.GetAllResolutions();
            DBDB.GetAllResolutions();
        }

        

        #region public NAVOTimePeriod SelectedPeriod { get; set; }

        static readonly PropertyChangedEventArgs SelectedPeriodChangedEventArgs = ObservableHelper.CreateArgs<NAVODataSources>(x => x.SelectedPeriod);
        NAVOTimePeriod _selectedPeriod;

        public NAVOTimePeriod SelectedPeriod
        {
            get { return _selectedPeriod; }
            set
            {
                if (_selectedPeriod == value) return;
                _selectedPeriod = value;
                NotifyPropertyChanged(SelectedPeriodChangedEventArgs);
            }
        }

        #endregion

        

        public BST BST { get; private set; }
        public DBDB DBDB { get; private set; }
        public GDEM GDEM { get; private set; }
        public SMGC SMGC { get; private set; }
        internal NAVOExtractionPacket ExtractionPacket { get; set; }
        internal NAVOConfiguration Configuration { get; set; }

        #region public List<NAVOTimePeriod> Months { get; set; }

        public List<NAVOTimePeriod> Months
        {
            get { return _months; }
            set
            {
                if (_months == value) return;
                _months = value;
                NotifyPropertyChanged(MonthsChangedEventArgs);
                SelectedPeriod = Months[0];
            }
        }

        static readonly PropertyChangedEventArgs MonthsChangedEventArgs = ObservableHelper.CreateArgs<NAVODataSources>(x => x.Months);
        List<NAVOTimePeriod> _months;

        #endregion



       


        public void ExtractAreas()
        {
            BST.TimePeriod = SelectedPeriod;
            DBDB.TimePeriod = SelectedPeriod;
            GDEM.TimePeriod = SelectedPeriod;
            SMGC.TimePeriod = SelectedPeriod;
            InterpretTimes(GDEM);
            InterpretTimes(SMGC);
            BST.ExtractArea(ExtractionPacket);
            DBDB.ExtractArea(ExtractionPacket);
            GDEM.ExtractArea(ExtractionPacket);
            SMGC.ExtractArea(ExtractionPacket);
        }

        void InterpretTimes(NAVODataSource dataSource)
        {
            switch (dataSource.TimePeriod)
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
                    dataSource.StartMonth = (int) dataSource.TimePeriod;
                    dataSource.EndMonth = (int) dataSource.TimePeriod;
                    dataSource.MonthsDuration = 1;
                    break;
                case NAVOTimePeriod.Spring:
                    dataSource.StartMonth = MonthMap[(int) Configuration.SpringStartMonth];
                    dataSource.EndMonth = MonthMap[(int) Configuration.SpringStartMonth + 3];
                    dataSource.MonthsDuration = 3;
                    break;
                case NAVOTimePeriod.Summer:
                    dataSource.StartMonth = MonthMap[(int) Configuration.SummerStartMonth];
                    dataSource.EndMonth = MonthMap[(int) Configuration.SummerStartMonth + 3];
                    dataSource.MonthsDuration = 3;
                    break;
                case NAVOTimePeriod.Fall:
                    dataSource.StartMonth = MonthMap[(int) Configuration.FallStartMonth];
                    dataSource.EndMonth = MonthMap[(int) Configuration.FallStartMonth + 3];
                    dataSource.MonthsDuration = 3;
                    break;
                case NAVOTimePeriod.Winter:
                    dataSource.StartMonth = MonthMap[(int) Configuration.WinterStartMonth];
                    dataSource.EndMonth = MonthMap[(int) Configuration.WinterStartMonth + 3];
                    dataSource.MonthsDuration = 3;
                    break;
                case NAVOTimePeriod.Cold:
                    dataSource.StartMonth = MonthMap[(int) Configuration.ColdSeasonStartMonth];
                    dataSource.EndMonth = MonthMap[(int) Configuration.ColdSeasonStartMonth + 6];
                    dataSource.MonthsDuration = 6;
                    break;
                case NAVOTimePeriod.Warm:
                    dataSource.StartMonth = MonthMap[(int) Configuration.WarmSeasonStartMonth];
                    dataSource.EndMonth = MonthMap[(int) Configuration.WarmSeasonStartMonth + 6];
                    dataSource.MonthsDuration = 6;
                    break;
            }
        }
    }
}
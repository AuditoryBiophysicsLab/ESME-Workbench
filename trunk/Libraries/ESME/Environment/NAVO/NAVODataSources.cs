using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using Cinch;

namespace ESME.Environment.NAVO
{
    public class NAVODataSources : ViewModelBase
    {
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
                       ExtractionProgramPath = Path.Combine(Path.GetDirectoryName(Assembly.GetCallingAssembly().Location), "ImportNetCDF.exe"),////todo: installer needs to put this in the right place.
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

        #region ExtractAreasCommand

        SimpleCommand<object, object> _extractAreas;

        public SimpleCommand<object, object> ExtractAreasCommand
        {
            get
            {
                return _extractAreas ?? (_extractAreas = new SimpleCommand<object, object>(delegate
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
                                                                                           }));
            }
        }

        #endregion

        public BST BST { get; private set; }
        public DBDB DBDB { get; private set; }
        public GDEM GDEM { get; private set; }
        public SMGC SMGC { get; private set; }
        internal NAVOExtractionPacket ExtractionPacket { get; set; }
        internal NAVOTimePeriod StartTime { get; set; }
        internal NAVOTimePeriod EndTime { get; set; }

        internal NAVOConfiguration Configuration { get; set; }


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
                    dataSource.StartMonth = (int) ExtractionPacket.TimePeriod;
                    dataSource.EndMonth = (int) ExtractionPacket.TimePeriod;
                    break;
                case NAVOTimePeriod.Spring:
                    dataSource.StartMonth = (int) Configuration.SpringStartMonth;
                    dataSource.EndMonth = (int) Configuration.SpringStartMonth + 3; //really?
                    break;
                case NAVOTimePeriod.Summer:
                    dataSource.StartMonth = (int) Configuration.SummerStartMonth;
                    dataSource.EndMonth = (int) Configuration.SummerStartMonth + 3;
                    break;
                case NAVOTimePeriod.Fall:
                    dataSource.StartMonth = (int) Configuration.FallStartMonth;
                    dataSource.EndMonth = (int) Configuration.FallStartMonth + 3;
                    break;
                case NAVOTimePeriod.Winter:
                    dataSource.StartMonth = (int) Configuration.WinterStartMonth;
                    dataSource.EndMonth = (int) Configuration.WinterStartMonth + 3;
                    break;
                case NAVOTimePeriod.Cold:
                    dataSource.StartMonth = (int) Configuration.ColdSeasonStartMonth;
                    dataSource.EndMonth = (int) Configuration.ColdSeasonStartMonth + 3;
                    break;
                case NAVOTimePeriod.Warm:
                    dataSource.StartMonth = (int) Configuration.WarmSeasonStartMonth;
                    dataSource.EndMonth = (int) Configuration.WarmSeasonStartMonth + 3;
                    break;
            }
        }
    }
}
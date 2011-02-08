using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
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

            SurfaceMarineGriddedClimatologyDatabase.DatabasePath = configurations.SMGCDirectory;
            SurfaceMarineGriddedClimatologyDatabase.ExtractionProgramPath = configurations.SMGCEXEPath;

            DigitalBathymetricDatabase.DatabasePath = configurations.DBDBDirectory;
            DigitalBathymetricDatabase.ExtractionProgramPath = configurations.DBDBEXEPath;
            DigitalBathymetricDatabase = new DigitalBathymetricDatabase();
            DigitalBathymetricDatabase.Initialize();

            BottomSedimentTypeDatabase.DatabasePath = configurations.BSTDirectory;
            BottomSedimentTypeDatabase.ExtractionProgramPath = configurations.BSTEXEPath;

            BottomSedimentTypeDatabase = new BottomSedimentTypeDatabase();
            BottomSedimentTypeDatabase.Initialize();

            var assemblyLocation = Assembly.GetCallingAssembly().Location;
            if (assemblyLocation == null) throw new ApplicationException("Assembly can't be null!");
            var extractionPath = Path.GetDirectoryName(assemblyLocation);
            if (extractionPath == null) throw new ApplicationException("Extraction path can't be null!");
            ////todo: installer needs to put this in the right place.
            GeneralizedDigitalEnvironmentModelDatabase.ExtractionProgramPath = Path.Combine(extractionPath, "ImportNetCDF.exe");
            GeneralizedDigitalEnvironmentModelDatabase.DatabasePath = configurations.GDEMDirectory;
        }

        public BottomSedimentTypeDatabase BottomSedimentTypeDatabase { get; private set; }
        public DigitalBathymetricDatabase DigitalBathymetricDatabase { get; private set; }
        public SurfaceMarineGriddedClimatologyDatabase SurfaceMarineGriddedClimatologyDatabase { get; private set; }
        internal NAVOExtractionPacket ExtractionPacket { get; set; }
        internal NAVOConfiguration Configuration { get; set; }

        public void ExtractAreas(IEnumerable<NAVOTimePeriod> timePeriods)
        {
            var selectedMonthIndices = new List<int>();
            foreach (var timePeriod in timePeriods)
                selectedMonthIndices.AddRange(GetMonthIndices(timePeriod));
            var uniqueMonths = selectedMonthIndices.Distinct().ToList();
            uniqueMonths.Sort();
            foreach (var month in uniqueMonths)
            {
                Status = "Extracting temperature and salinity data for " + (NAVOTimePeriod) month;
                GeneralizedDigitalEnvironmentModelDatabase.ExtractAreaFromMonthFile(ExtractionPacket.Filename, ExtractionPacket.North, ExtractionPacket.South, ExtractionPacket.East, ExtractionPacket.West, month);
            }

            foreach (var timePeriod in timePeriods)
            {
                var monthIndices = GetMonthIndices(timePeriod);
                if (monthIndices.Count() <= 1) continue;
                Status = "Creating average temperature and salinity data for " + timePeriod;
                GeneralizedDigitalEnvironmentModelDatabase.AverageMonthlyData(ExtractionPacket.Filename, monthIndices, timePeriod);
            }

            foreach (var timePeriod in timePeriods)
            {
                Status = "Creating sound speed data for " + timePeriod;
                GeneralizedDigitalEnvironmentModelDatabase.CreateSoundSpeedFile(ExtractionPacket.Filename, timePeriod);
            }

            // BST and DBDB should not need the period to be provided, as these datasets are time-invariant
            Status = "Extracting sediment data for selected area";
            BottomSedimentTypeDatabase.ExtractArea(ExtractionPacket.Filename, BottomSedimentTypeDatabase.SelectedResolution, ExtractionPacket.North, ExtractionPacket.South, ExtractionPacket.East, ExtractionPacket.West);

            Status = "Extracting bathymetry data for selected area";
            DigitalBathymetricDatabase.ExtractArea(ExtractionPacket.Filename, DigitalBathymetricDatabase.SelectedResolution, ExtractionPacket.North, ExtractionPacket.South, ExtractionPacket.East, ExtractionPacket.West, DigitalBathymetricDatabase.Resolutions);

            foreach (var timePeriod in timePeriods)
            {
                var monthIndices = GetMonthIndices(timePeriod);
                Status = "Extracting wind data for " + timePeriod;
                SurfaceMarineGriddedClimatologyDatabase.ExtractArea(ExtractionPacket.Filename, timePeriod, monthIndices.First(), monthIndices.Last(), monthIndices.Count(), ExtractionPacket.North, ExtractionPacket.South, ExtractionPacket.East, ExtractionPacket.West);
            }
        }

        #region public string Status { get; set; }

        public string Status
        {
            get { return _status; }
            set
            {
                if (_status == value) return;
                _status = value;
                NotifyPropertyChanged(StatusChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs StatusChangedEventArgs = ObservableHelper.CreateArgs<NAVODataSources>(x => x.Status);
        string _status;

        #endregion

        IEnumerable<int> GetMonthIndices(NAVOTimePeriod timePeriod)
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
                    yield return (int) timePeriod;
                    yield break;
                case NAVOTimePeriod.Spring:
                    yield return MonthMap[(int) Configuration.SpringStartMonth];
                    yield return MonthMap[(int) Configuration.SpringStartMonth + 1];
                    yield return MonthMap[(int) Configuration.SpringStartMonth + 2];
                    yield break;
                case NAVOTimePeriod.Summer:
                    yield return MonthMap[(int) Configuration.SummerStartMonth];
                    yield return MonthMap[(int) Configuration.SummerStartMonth + 1];
                    yield return MonthMap[(int) Configuration.SummerStartMonth + 2];
                    yield break;
                case NAVOTimePeriod.Fall:
                    yield return MonthMap[(int) Configuration.FallStartMonth];
                    yield return MonthMap[(int) Configuration.FallStartMonth + 1];
                    yield return MonthMap[(int) Configuration.FallStartMonth + 2];
                    yield break;
                case NAVOTimePeriod.Winter:
                    yield return MonthMap[(int) Configuration.WinterStartMonth];
                    yield return MonthMap[(int) Configuration.WinterStartMonth + 1];
                    yield return MonthMap[(int) Configuration.WinterStartMonth + 2];
                    yield break;
                case NAVOTimePeriod.Cold:
                    yield return MonthMap[(int) Configuration.ColdSeasonStartMonth];
                    yield return MonthMap[(int) Configuration.ColdSeasonStartMonth + 1];
                    yield return MonthMap[(int) Configuration.ColdSeasonStartMonth + 2];
                    yield return MonthMap[(int) Configuration.ColdSeasonStartMonth + 3];
                    yield return MonthMap[(int) Configuration.ColdSeasonStartMonth + 4];
                    yield return MonthMap[(int) Configuration.ColdSeasonStartMonth + 5];
                    yield break;
                case NAVOTimePeriod.Warm:
                    yield return MonthMap[(int) Configuration.WarmSeasonStartMonth];
                    yield return MonthMap[(int) Configuration.WarmSeasonStartMonth + 1];
                    yield return MonthMap[(int) Configuration.WarmSeasonStartMonth + 2];
                    yield return MonthMap[(int) Configuration.WarmSeasonStartMonth + 3];
                    yield return MonthMap[(int) Configuration.WarmSeasonStartMonth + 4];
                    yield return MonthMap[(int) Configuration.WarmSeasonStartMonth + 5];
                    yield break;
            }
        }
    }
}
using System;
using System.IO;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
    public static class NAVODatabases
    {
        public static SoundSpeed ImportGDEM(GeoRect geoRect, string destinationFilename, NAVOTimePeriod timePeriod, bool isTemperature)
        {
            CheckDestinationDirectory(destinationFilename);

            var dataField = isTemperature ? GDEM.ReadFile(GDEM.FindTemperatureFile(timePeriod), "water_temp", timePeriod, geoRect) :
                                            GDEM.ReadFile(GDEM.FindSalinityFile(timePeriod), "salinity", timePeriod, geoRect);
            var data = new SoundSpeed();
            data.SoundSpeedFields.Add(dataField);
            data.Serialize(destinationFilename);
            return data;
        }

        public static Sediment ImportBST(GeoRect geoRect, string destinationFilename)
        {
            CheckDestinationDirectory(destinationFilename);

            var sediment = BST.Extract(geoRect);
            sediment.Save(destinationFilename);
            return sediment;
        }

        public static Wind ImportSMGC(GeoRect geoRect, string destinationFilename)
        {
            CheckDestinationDirectory(destinationFilename);
            var wind = SMGC.Import(geoRect);
            wind.Save(destinationFilename);
            return wind;
        }

        public static Bathymetry ImportDBDB(GeoRect geoRect, string destinationFilename, float resolution)
        {
            CheckDestinationDirectory(destinationFilename);

            var bathymetry = DBDB.Extract(resolution, geoRect);
            bathymetry.Save(destinationFilename);
            return bathymetry;
        }

        public static BottomLoss ImportBottomLoss(GeoRect geoRect, string destinationFilename)
        {
            CheckDestinationDirectory(destinationFilename);
            var bottomLoss = BottomLossDatabase.Extract(geoRect);
            bottomLoss.Save(destinationFilename);
            return bottomLoss;
        }

        static void CheckDestinationDirectory(string destinationFilename)
        {
            if (string.IsNullOrEmpty(destinationFilename)) throw new ArgumentNullException("destinationFilename");
            var destinationDirectory = Path.GetDirectoryName(destinationFilename);
            if (string.IsNullOrEmpty(destinationDirectory)) throw new ArgumentException("Destination filename must contain a full path", "destinationFilename");
            if (!Directory.Exists(destinationDirectory)) Directory.CreateDirectory(destinationDirectory);
        }
    }
}

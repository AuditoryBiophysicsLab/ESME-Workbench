using System;
using System.IO;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
    public static class NAVODatabases
    {
        public static SoundSpeed ImportGDEM(GeoRect geoRect, NAVOTimePeriod timePeriod, bool isTemperature)
        {
            var dataField = isTemperature ? GDEM.ReadFile(GDEM.FindTemperatureFile(timePeriod), "water_temp", timePeriod, geoRect) :
                                            GDEM.ReadFile(GDEM.FindSalinityFile(timePeriod), "salinity", timePeriod, geoRect);
            var data = new SoundSpeed();
            data.SoundSpeedFields.Add(dataField);
            return data;
        }

        public static Sediment ImportBST(GeoRect geoRect)
        {
            var sediment = BST.Extract(geoRect);
            return sediment;
        }

        public static Wind ImportSMGC(GeoRect geoRect)
        {
            var wind = SMGC.Import(geoRect);
            return wind;
        }

        public static Bathymetry ImportDBDB(GeoRect geoRect, float resolution)
        {
            return DBDB.Extract(resolution, geoRect);
        }

        public static BottomLoss ImportBottomLoss(GeoRect geoRect)
        {
            return BottomLossDatabase.Extract(geoRect);
        }
    }
}

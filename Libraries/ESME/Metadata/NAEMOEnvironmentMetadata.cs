using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using Cinch;
using ESME.Environment.NAVO;
using ESME.TransmissionLoss.CASS;
using HRC.Navigation;

namespace ESME.Metadata
{
    public class NAEMOEnvironmentMetadata : NAEMOMetadataBase
    {
        public NAEMOEnvironmentMetadata() {  }

        new internal static readonly List<Type> ReferencedTypes = new List<Type>(NAEMOMetadataBase.ReferencedTypes) { typeof(NAVOTimePeriod), typeof(NAEMOEnvironmentLocation), typeof(NAEMOEnvironmentFile) };

        public static NAEMOEnvironmentMetadata FromEnvironmentFile(string naemoEnvironmentFilename)
        {
            var metaDataFilename = Path.Combine(Path.GetDirectoryName(naemoEnvironmentFilename), Path.GetFileNameWithoutExtension(naemoEnvironmentFilename) + ".xml");
            var environmentFile = NAEMOEnvironmentFile.Load(naemoEnvironmentFilename);
            var result = new NAEMOEnvironmentMetadata
            {
                TimePeriod = environmentFile.TimePeriod,
                Filename = metaDataFilename,
            };
            result.Locations.AddRange(environmentFile.Locations);

            return result;
        }

        public static NAEMOEnvironmentMetadata Load(string metaDataFilename) { return Load<NAEMOEnvironmentMetadata>(metaDataFilename); }

        public void Save(string filename = null) { Save(this, ReferencedTypes, filename); }

        #region public NAVOTimePeriod TimePeriod { get; set; }

        public NAVOTimePeriod TimePeriod
        {
            get { return _timePeriod; }
            set
            {
                if (_timePeriod == value) return;
                _timePeriod = value;
                NotifyPropertyChanged(TimePeriodChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TimePeriodChangedEventArgs = ObservableHelper.CreateArgs<NAEMOEnvironmentMetadata>(x => x.TimePeriod);
        NAVOTimePeriod _timePeriod;

        #endregion

        #region public List<EarthCoordinate> Locations { get; set; }

        public List<EarthCoordinate> Locations
        {
            get { return _locations ?? (_locations = new List<EarthCoordinate>()); }
            set
            {
                if (_locations == value) return;
                _locations = value;
                NotifyPropertyChanged(LocationsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LocationsChangedEventArgs = ObservableHelper.CreateArgs<NAEMOEnvironmentMetadata>(x => x.Locations);
        List<EarthCoordinate> _locations;

        #endregion

    }
}
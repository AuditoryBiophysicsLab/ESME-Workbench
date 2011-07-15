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

        new internal static readonly List<Type> ReferencedTypes = new List<Type>(NAEMOMetadataBase.ReferencedTypes);

        public static NAEMOEnvironmentMetadata FromEnvironmentFile(string naemoEnvironmentFilename)
        {
            var metaDataFilename = Path.Combine(Path.GetDirectoryName(naemoEnvironmentFilename), Path.GetFileNameWithoutExtension(naemoEnvironmentFilename) + ".xml");
            NAEMOEnvironmentMetadata result = null;
            try
            {
                var environmentFile = NAEMOEnvironmentFile.Load(naemoEnvironmentFilename);
                if (environmentFile == null) return null;
                result = new NAEMOEnvironmentMetadata
                {
                    TimePeriod = environmentFile.TimePeriod,
                    Filename = metaDataFilename,
                };
            }
            catch
            {
                File.Delete(naemoEnvironmentFilename);
            }

            return result;
        }

        public static NAEMOEnvironmentMetadata Load(string metaDataFilename) { return Load<NAEMOEnvironmentMetadata>(metaDataFilename, ReferencedTypes); }

        public void Save(string filename = null) { Save(this, ReferencedTypes, filename); }

        #region public string BathymetryName { get; set; }

        public string BathymetryName
        {
            get { return _bathymetryName; }
            set
            {
                if (_bathymetryName == value) return;
                _bathymetryName = value;
                NotifyPropertyChanged(BathymetryNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BathymetryNameChangedEventArgs = ObservableHelper.CreateArgs<NAEMOEnvironmentMetadata>(x => x.BathymetryName);
        string _bathymetryName;

        #endregion

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
    }
}
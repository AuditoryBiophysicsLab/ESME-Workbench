using System;
using System.Collections.Generic;
using System.ComponentModel;
using Cinch;
using ESME.Environment.NAVO;

namespace ESME.Metadata
{
    public class NAEMOEnvironmentMetadata : NAEMOMetadataBase
    {
        new internal static readonly List<Type> ReferencedTypes = new List<Type>(NAEMOMetadataBase.ReferencedTypes) {typeof(NAVOTimePeriod)};

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

        #region public string OverlayFilename { get; set; }

        public string OverlayFilename
        {
            get { return _overlayFilename; }
            set
            {
                if (_overlayFilename == value) return;
                _overlayFilename = value;
                NotifyPropertyChanged(OverlayNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OverlayNameChangedEventArgs = ObservableHelper.CreateArgs<NAEMOEnvironmentMetadata>(x => x.OverlayFilename);
        string _overlayFilename;

        #endregion
    }
}
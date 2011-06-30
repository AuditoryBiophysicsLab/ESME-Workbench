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


    }
}
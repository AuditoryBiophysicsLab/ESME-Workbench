using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using Cinch;
using ESME.Metadata;
using HRC.Utility;

namespace ESME.Views.Locations
{
    public class MetadataPropertiesViewModel : ViewModelBase
    {
        public MetadataPropertiesViewModel(NAEMOBathymetryMetadata bathymetryMetadata = null, NAEMOEnvironmentMetadata environmentMetadata = null, NAEMOScenarioMetadata naemoScenarioMetadata = null)
        {
            
        }

        #region public bool IsBathymetryDisplay { get; set; }

        public bool IsBathymetryDisplay
        {
            get { return _isBathymetryDisplay; }
            set
            {
                if (_isBathymetryDisplay == value) return;
                _isBathymetryDisplay = value;
                NotifyPropertyChanged(IsBathymetryDisplayChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsBathymetryDisplayChangedEventArgs = ObservableHelper.CreateArgs<MetadataPropertiesViewModel>(x => x.IsBathymetryDisplay);
        bool _isBathymetryDisplay;

        #endregion

    }
}

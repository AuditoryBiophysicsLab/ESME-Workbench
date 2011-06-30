using System;
using System.Collections.Generic;
using System.ComponentModel;
using Cinch;

namespace ESME.Metadata
{
    public class NAEMOScenarioMetadata : NAEMOMetadataBase
    {
        new internal static readonly List<Type> ReferencedTypes = new List<Type>(NAEMOMetadataBase.ReferencedTypes) {};

        #region public string BathymetryFilename { get; set; }

        public string BathymetryFilename
        {
            get { return _bathymetryFilename; }
            set
            {
                if (_bathymetryFilename == value) return;
                _bathymetryFilename = value;
                NotifyPropertyChanged(BathymetryFilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BathymetryFilenameChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.BathymetryFilename);
        string _bathymetryFilename;

        #endregion

        #region public string EnvironmentFilename { get; set; }

        public string EnvironmentFilename
        {
            get { return _environmentFilename; }
            set
            {
                if (_environmentFilename == value) return;
                _environmentFilename = value;
                NotifyPropertyChanged(EnvironmentFilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs EnvironmentFilenameChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.EnvironmentFilename);
        string _environmentFilename;

        #endregion
    }
}

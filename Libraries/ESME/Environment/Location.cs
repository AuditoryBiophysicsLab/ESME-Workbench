using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using Cinch;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment
{
    public class Location : PropertyChangedBase
    {
        static readonly List<Type> ReferencedTypes = new List<Type>(LocationMetadata.ReferencedTypes);

        public LocationMetadata LocationMetadata { get; set; }

        public Location() { LocationMetadata = new LocationMetadata(); }

        public static Location Load(string filename)
        {
            return new Location { LocationMetadata = XmlSerializer<LocationMetadata>.Load(filename, ReferencedTypes) };
        }

        public void Save(string filename)
        {
            var serializer = new XmlSerializer<LocationMetadata> { Data = LocationMetadata };
            serializer.Save(filename, ReferencedTypes);
        }

        public static List<Location> AvailableLocations(string scenarioDirectory)
        {
            var locationDirectories = Directory.GetDirectories(scenarioDirectory);
            return (from location in locationDirectories
                    where File.Exists(Path.Combine(location, "location.xml"))
                    select Load(Path.Combine(location, "location.xml"))).ToList();
        }
    }


    public class LocationMetadata : PropertyChangedBase
    {
        public static readonly List<Type> ReferencedTypes = new List<Type> { typeof (GeoRect), typeof(AvailableResolutions), typeof(AvailableResolution), typeof(AvailableTimePeriods), typeof (AvailableTimePeriod) };

        #region public string Name { get; set; }

        public string Name
        {
            get { return _name; }
            set
            {
                if (_name == value) return;
                _name = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<LocationMetadata>(x => x.Name);
        private string _name;

        #endregion

        #region public GeoRect Bounds { get; set; }

        public GeoRect Bounds
        {
            get { return _bounds; }
            set
            {
                if (_bounds == value) return;
                _bounds = value;
                NotifyPropertyChanged(BoundsChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs BoundsChangedEventArgs = ObservableHelper.CreateArgs<LocationMetadata>(x => x.Bounds);
        private GeoRect _bounds;

        #endregion

        #region public AvailableResolutions AvailableResolutions { get; set; }

        public AvailableResolutions AvailableResolutions
        {
            get { return _availableResolutions; }
            set
            {
                if (_availableResolutions == value) return;
                _availableResolutions = value;
                NotifyPropertyChanged(AvailableResolutionsChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs AvailableResolutionsChangedEventArgs = ObservableHelper.CreateArgs<LocationMetadata>(x => x.AvailableResolutions);
        private AvailableResolutions _availableResolutions;

        #endregion

        #region public AvailableTimePeriods AvailableTimePeriods { get; set; }

        public AvailableTimePeriods AvailableTimePeriods
        {
            get { return _availableTimePeriods; }
            set
            {
                if (_availableTimePeriods == value) return;
                _availableTimePeriods = value;
                NotifyPropertyChanged(AvailableTimePeriodsChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs AvailableTimePeriodsChangedEventArgs = ObservableHelper.CreateArgs<LocationMetadata>(x => x.AvailableTimePeriods);
        private AvailableTimePeriods _availableTimePeriods;

        #endregion
    }
}

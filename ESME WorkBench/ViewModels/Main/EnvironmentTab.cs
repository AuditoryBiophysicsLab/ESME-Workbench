using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Windows;
using Cinch;
using ESME.Environment;
using ESME.Views.Locations;
using HRC.Utility;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        #region Bindable Properties

        #region public float BufferZoneSize { get; set; }

        public float BufferZoneSize
        {
            get { return _bufferZoneSize; }
            set
            {
                if (_bufferZoneSize == value) return;
                _bufferZoneSize = value;
                NotifyPropertyChanged(BufferZoneSizeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BufferZoneSizeChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.BufferZoneSize);
        float _bufferZoneSize;

        #endregion

        #region public List<GeoDistanceUnit> DistanceUnits { get; set; }

        public List<GeoDistanceUnit> DistanceUnits
        {
            get
            {
                return _distanceUnits ?? (_distanceUnits = new List<GeoDistanceUnit>
                {
                        new GeoDistanceUnit {Name = "kilometers", ScaleToMeters = 1000},
                        new GeoDistanceUnit {Name = "meters", ScaleToMeters = 1},
                        new GeoDistanceUnit {Name = "nautical miles", ScaleToMeters = 1852}
                });
            }
            set
            {
                if (_distanceUnits == value) return;
                _distanceUnits = value;
                NotifyPropertyChanged(DistanceUnitsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DistanceUnitsChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.DistanceUnits);
        List<GeoDistanceUnit> _distanceUnits;

        #endregion

        #region public Visibility LocationEditorContextIsVisible { get; set; }

        public Visibility LocationEditorContextIsVisible
        {
            get { return LocationIsSelected ? Visibility.Visible : Visibility.Collapsed; }
        }

        static readonly PropertyChangedEventArgs LocationEditorContextIsVisibleChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.LocationEditorContextIsVisible);

        #endregion

        #region public GeoDistanceUnit SelectedDistanceUnit { get; set; }

        public GeoDistanceUnit SelectedDistanceUnit
        {
            get { return _selectedDistanceUnit ?? (_selectedDistanceUnit = DistanceUnits[0]); }
            set
            {
                _selectedDistanceUnit = value;
                NotifyPropertyChanged(SelectedDistanceUnitChangedEventArgs);
                Debug.WriteLine(string.Format("SelectedDistanceUnit is now {0}", SelectedDistanceUnit == null ? "NULL" : SelectedDistanceUnit.Name));
            }
        }

        static readonly PropertyChangedEventArgs SelectedDistanceUnitChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedDistanceUnit);
        GeoDistanceUnit _selectedDistanceUnit;

        #endregion

        #region public List<Location> AvailableLocations { get; set; }

        public List<Location> AvailableLocations
        {
            get
            {
                return _availableLocations ?? (_availableLocations = Location.AvailableLocations(Globals.AppSettings.ScenarioDataDirectory));
            }
        }

        static readonly PropertyChangedEventArgs AvailableLocationsChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.AvailableLocations);
        List<Location> _availableLocations;

        void UpdateAvailableLocations()
        {
            _availableLocations = Location.AvailableLocations(Globals.AppSettings.ScenarioDataDirectory);
            NotifyPropertyChanged(AvailableLocationsChangedEventArgs);
            NotifyPropertyChanged(LocationsAreAvailableChangedEventArgs);
        }

        #endregion

        #region public bool LocationsAreAvailable { get; set; }

        public bool LocationsAreAvailable
        {
            get { return AvailableLocations.Count > 0; }
        }

        static readonly PropertyChangedEventArgs LocationsAreAvailableChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.LocationsAreAvailable);

        #endregion

        #region public bool LocationIsSelected { get; set; }

        public bool LocationIsSelected
        {
            get { return Location != null; }
        }

        static readonly PropertyChangedEventArgs LocationIsSelectedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.LocationIsSelected);

        #endregion

        #region public Location Location { get; set; }

        public Location Location
        {
            get { return _location; }
            set
            {
                if (_location == value) return;
                _location = value;
                NotifyPropertyChanged(LocationIsSelectedChangedEventArgs);
                NotifyPropertyChanged(LocationChangedEventArgs);
                NotifyPropertyChanged(LocationEditorContextIsVisibleChangedEventArgs);
                SelectedDistanceUnit = DistanceUnits[0];
            }
        }

        static readonly PropertyChangedEventArgs LocationChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.Location);
        Location _location;

        #endregion

        #endregion

        #region Commands

        #region NewLocationCommand

        public SimpleCommand<object, object> NewLocationCommand
        {
            get
            {
                return _newLocation ??
                       (_newLocation =
                        new SimpleCommand<object, object>(delegate { NewLocationHandler(); }));
            }
        }

        private SimpleCommand<object, object> _newLocation;

        void NewLocationHandler()
        {
            var vm = new NewLocationViewModel(Globals.AppSettings);
            var result = _visualizerService.ShowDialog("NewLocationView", vm);
            if ((result.HasValue) && (result.Value))
            {
                UpdateAvailableLocations();
                Location = Location.Load(Path.Combine(vm.LocationPath, "location.xml"));
            }
        }
        #endregion

        #endregion
    }
}

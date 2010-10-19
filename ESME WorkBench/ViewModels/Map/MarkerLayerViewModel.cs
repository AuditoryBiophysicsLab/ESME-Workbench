using System;
using System.Linq;
using System.Windows.Media.Imaging;
using System.Xml.Serialization;
using HRC.Navigation;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Map
{
    public class MarkerLayerViewModel : MapLayerViewModel
    {
        readonly SimpleMarkerOverlay _markerOverlay;

        #region public constructor

        public MarkerLayerViewModel()
        {
            _markerOverlay = new SimpleMarkerOverlay();
            Overlay = _markerOverlay;
        }

        #endregion

        #region public Uri MarkerImageUri { get; set; }

        [XmlIgnore]
        public Uri MarkerImageUri
        {
            get { return _markerImageUri; }
            set
            {
                if (_markerImageUri == value) return;
                _markerImageUri = value;
                _imageSource = new BitmapImage(_markerImageUri);
            }
        }

        Uri _markerImageUri;
        BitmapImage _imageSource;

        #endregion

        public Marker AddMarker(EarthCoordinate location, object tag)
        {
            var newMarker = new Marker(location.Longitude_degrees, location.Latitude_degrees)
                            {
                                ImageSource = _imageSource,
                                Tag = tag,
                                YScreenOffset = -(_imageSource.Height/2),
                            };
            _markerOverlay.Markers.Add(newMarker);
            return newMarker;
        }

        public void RemoveMarker(object tag)
        {
            foreach (var marker in _markerOverlay.Markers.Where(marker => marker.Tag == tag))
            {
                _markerOverlay.Markers.Remove(marker);
                break;
            }
        }
    }
}
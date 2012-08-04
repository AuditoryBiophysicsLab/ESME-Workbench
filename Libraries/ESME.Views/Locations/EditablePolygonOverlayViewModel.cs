using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using HRC.Navigation;
using HRC.ViewModels;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESME.Views.Locations
{
    public class EditablePolygonOverlayViewModel : ViewModelBase, INotifyPropertyChanged
    {
        readonly WpfMap _wpfMap;
        EditInteractiveOverlay _editOverlay;

        public EditablePolygonOverlayViewModel(WpfMap wpfMap)
        {
            _wpfMap = wpfMap;
        }

        void UpdateOverlay()
        {
            if (!_isVisible) return;
            _wpfMap.EditOverlay.EditShapesLayer.InternalFeatures.Clear();
            var polygon = new Feature(new PolygonShape(GeoArray.ToWellKnownText()));
            _wpfMap.EditOverlay.EditShapesLayer.InternalFeatures.Add(polygon);
            _wpfMap.EditOverlay.CalculateAllControlPoints();
            _wpfMap.Refresh();
        }

        bool _isVisible;
        public bool IsVisible
        {
            get { return _isVisible; }
            set
            {
                _isVisible = value;
                if (!_isVisible)
                {
                    _wpfMap.EditOverlay.EditShapesLayer.InternalFeatures.Clear();
                    _wpfMap.Refresh();
                    return;
                }
                _editOverlay = new EditInteractiveOverlay { CanAddVertex = true, CanRotate = true };
                _editOverlay.FeatureDragged += (sender, args) => CheckOverlayWithinBounds(args.DraggedFeature);
                _editOverlay.VertexMoved += (sender, args) => CheckOverlayWithinBounds(args.AffectedVertexFeature);
                _editOverlay.FeatureResized += (sender, args) => CheckOverlayWithinBounds(args.ResizedFeature);
                _editOverlay.FeatureRotated += (sender, args) => CheckOverlayWithinBounds(args.RotatedFeature);
                var polygon = new Feature(new PolygonShape(GeoArray.ToWellKnownText()));
                _editOverlay.EditShapesLayer.InternalFeatures.Add(polygon);
                _editOverlay.CalculateAllControlPoints();
                _wpfMap.EditOverlay = _editOverlay;
                _wpfMap.Refresh();
            }
        }

        void CheckOverlayWithinBounds(Feature feature)
        {
            var errors = new List<string>();
            _geoArray = GeoArray.FromWellKnownText(feature.GetWellKnownText());
            OnPropertyChanged("GeoArray");
#if false
            if (_writeKML)
            {
                var kml = new KMLRoot();
                var folder = new Folder("Overlay Segment Test");
                var segments = _geoArray.Segments.ToArray();
                for (var segment = 0; segment < segments.Length; segment++)
                {
                    segments[segment].Placemark.name = string.Format("Segment {0}", segment);
                    folder.Add(segments[segment].Placemark);
                }
                kml.Document.Add(folder);
                var savePath = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "Overlay Segment Tests", "OverlaySegmentTest.kml");
                kml.Save(savePath);
                _writeKML = false;
            }
#endif
            if (_geoArray.HasCrossingSegments && !AreCrossingSegmentsAllowed) errors.Add("Perimeter segments may not cross");
            if (_locationBoundsRectangle != null && !_locationBoundsRectangle.Contains(feature.GetShape())) errors.Add("Perimeter must be within location bounds");
            var areaStyle = errors.Count > 0 ? _errorAreaStyle : _normalAreaStyle;
            _wpfMap.EditOverlay.EditShapesLayer.ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = areaStyle;
            _wpfMap.EditOverlay.EditShapesLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            Errors = errors.Count == 0 ? string.Empty : string.Join("\n", errors);
            HasErrors = errors.Count > 0;
            if (HasErrors) Debug.WriteLine(string.Format("Errors: {0}", Errors));
        }

        readonly AreaStyle _normalAreaStyle = new AreaStyle(new GeoPen(GeoColor.StandardColors.Blue), new GeoSolidBrush(new GeoColor(128, 0, 0, 128)));
        readonly AreaStyle _errorAreaStyle = new AreaStyle(new GeoPen(GeoColor.StandardColors.Red), new GeoSolidBrush(new GeoColor(128, 128, 0, 0)));

        public bool HasErrors { get; set; }
        public string Errors { get; set; }
        public bool AreCrossingSegmentsAllowed { get; set; }
        //bool _writeKML;

        GeoArray _geoArray;
        public GeoArray GeoArray
        {
            get
            {
                //_wpfMap.EditOverlay.EditShapesLayer.InternalFeatures
                return _geoArray;
            }
            set
            {
                _geoArray = value;
                UpdateOverlay();
            }
        }

        RectangleShape _locationBoundsRectangle;
        GeoRect _locationBounds;
        public GeoRect LocationBounds { get { return _locationBounds; } set
        {
            _locationBounds = value;
            _locationBoundsRectangle = new RectangleShape(_locationBounds.West, _locationBounds.North, _locationBounds.East, _locationBounds.South);
        } }
    }
}
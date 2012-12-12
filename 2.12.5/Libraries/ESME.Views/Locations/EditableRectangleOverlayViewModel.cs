using System.ComponentModel;
using ESME.Mapping;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Validation;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESME.Views.Locations
{
    public class EditableRectangleOverlayViewModel : ValidatingViewModel, INotifyPropertyChanged
    {
        readonly WpfMap _wpfMap;
        EditInteractiveOverlay _editOverlay;

        public EditableRectangleOverlayViewModel(WpfMap wpfMap)
        {
            _wpfMap = wpfMap;
            AddValidationRules(
                new ValidationRule<EditableRectangleOverlayViewModel>
                {
                    PropertyName = "North",
                    Description = "Must be between -90 and +90 and be greater than South",
                    IsRuleValid = (target, rule) => target.North >= -90 && target.North <= 90 && target.North > target.South,
                },
                new ValidationRule<EditableRectangleOverlayViewModel>
                {
                    PropertyName = "South",
                    Description = "Must be between -90 and +90 and be less than North",
                    IsRuleValid = (target, rule) => target.South >= -90 && target.South <= 90 && target.North > target.South,
                },
                new ValidationRule<EditableRectangleOverlayViewModel>
                {
                    PropertyName = "East",
                    Description = "Must be between -180 and +180 and be greater than West",
                    IsRuleValid = (target, rule) => target.East >= -180 && target.East <= 180 && target.East > target.West,
                },
                new ValidationRule<EditableRectangleOverlayViewModel>
                {
                    PropertyName = "West",
                    Description = "Must be between -180 and +180 and be less than East",
                    IsRuleValid = (target, rule) => target.West >= -180 && target.West <= 180 && target.East > target.West,
                });
        }

        void UpdateOverlay()
        {
            Validate();
            if (!_isVisible || !IsValid) return;
            _wpfMap.EditOverlay.EditShapesLayer.InternalFeatures.Clear();
            var rectangle = new Feature(new RectangleShape(West, North, East, South));
            rectangle.ColumnValues.Add("Edit", null);
            _wpfMap.EditOverlay.EditShapesLayer.InternalFeatures.Add(rectangle);
            _wpfMap.EditOverlay.EditShapesLayer.Open();
            _wpfMap.EditOverlay.EditShapesLayer.Columns.Add(new FeatureSourceColumn("Edit"));
            _wpfMap.EditOverlay.EditShapesLayer.Close();
            _wpfMap.EditOverlay.CalculateAllControlPoints();
            _wpfMap.Refresh();
        }

        void UpdateCoordinates(RectangleShape bounds)
        {
            _north = bounds.UpperLeftPoint.Y;
            OnPropertyChanged("North");
            _south = bounds.LowerRightPoint.Y;
            OnPropertyChanged("South");
            _east = bounds.LowerRightPoint.X;
            OnPropertyChanged("East");
            _west = bounds.UpperLeftPoint.X;
            OnPropertyChanged("West");
        }

        void FeatureDraggedHandler(object sender, FeatureDraggedEditInteractiveOverlayEventArgs featureDraggedEditInteractiveOverlayEventArgs) 
        {
            _editOverlay.EditShapesLayer.Open();
            var bounds = _editOverlay.EditShapesLayer.GetBoundingBox();
            _editOverlay.EditShapesLayer.Close();
            UpdateCoordinates(bounds);
            //Debug.WriteLine("Dragged: North {0} South {1} East {2} West {3}", bounds.UpperLeftPoint.Y, bounds.LowerRightPoint.Y, bounds.LowerRightPoint.X, bounds.UpperLeftPoint.X);
        }

        public void FeatureResizedHandler(object sender, FeatureResizedEditInteractiveOverlayEventArgs args)
        {
            _editOverlay.EditShapesLayer.Open();
            var bounds = _editOverlay.EditShapesLayer.GetBoundingBox();
            _editOverlay.EditShapesLayer.Close();
            UpdateCoordinates(bounds);
            //Debug.WriteLine("Resized: North {0} South {1} East {2} West {3}", bounds.UpperLeftPoint.Y, bounds.LowerRightPoint.Y, bounds.LowerRightPoint.X, bounds.UpperLeftPoint.X);
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
                    _editOverlay.FeatureResized -= FeatureResizedHandler;
                    _editOverlay.FeatureDragged -= FeatureDraggedHandler;
                    _wpfMap.EditOverlay.EditShapesLayer.InternalFeatures.Clear();
                    _wpfMap.Refresh();
                    return;
                }
                _editOverlay = new EditableRectangleInteractiveOverlay {CanAddVertex = false, CanRotate = false};
                var rectangle = new Feature(new RectangleShape(West, North, East, South));
                rectangle.ColumnValues.Add("Edit", null);
                _editOverlay.EditShapesLayer.InternalFeatures.Add(rectangle);
                _editOverlay.EditShapesLayer.Open();
                _editOverlay.EditShapesLayer.Columns.Add(new FeatureSourceColumn("Edit"));
                _editOverlay.EditShapesLayer.Close();
                _editOverlay.CalculateAllControlPoints();
                _editOverlay.FeatureResized += FeatureResizedHandler;
                _editOverlay.FeatureDragged += FeatureDraggedHandler;
                _wpfMap.EditOverlay = _editOverlay;
                // Draw the map image on the screen
                _wpfMap.Refresh();
            }
        }

        double _north;
        [Initialize( 10.0)] public double North
        {
            get { return _north; }
            set
            {
                _north = value;
                UpdateOverlay();
            }
        }

        double _south;
        [Initialize(-10.0)] public double South
        {
            get { return _south; }
            set
            {
                _south = value;
                UpdateOverlay();
            }
        }

        double _east;
        [Initialize( 10.0)] public double East
        {
            get { return _east; }
            set
            {
                _east = value;
                UpdateOverlay();
            }
        }

        double _west;
        [Initialize(-10.0)] public double West
        {
            get { return _west; }
            set
            {
                _west = value;
                UpdateOverlay();
            }
        }

        public GeoRect GeoRect { get { return new GeoRect(North, South, East, West); } }
    }
}
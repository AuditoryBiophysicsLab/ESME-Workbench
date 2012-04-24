using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using ESME.Mapping;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Validation;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESME.Views.Locations
{
    public class EditOverlayViewModel : ValidatingViewModel, INotifyPropertyChanged
    {
        readonly WpfMap _wpfMap;
        CustomEditInteractiveOverlay _editOverlay;

        public EditOverlayViewModel(WpfMap wpfMap)
        {
            _wpfMap = wpfMap;
            PropertyChanged += (s, e) =>
            {
                switch (e.PropertyName)
                {
                    case "North":
                    case "South":
                    case "East":
                    case "West":
                        if (!_isVisible) return;

                        _wpfMap.EditOverlay.EditShapesLayer.InternalFeatures.Clear();
                        var rectangle = new Feature(new RectangleShape(West, North, East, South));
                        rectangle.ColumnValues.Add("Edit", null);
                        _wpfMap.EditOverlay.EditShapesLayer.InternalFeatures.Add(rectangle);
                        _wpfMap.EditOverlay.EditShapesLayer.Open();
                        _wpfMap.EditOverlay.EditShapesLayer.Columns.Add(new FeatureSourceColumn("Edit"));
                        _wpfMap.EditOverlay.EditShapesLayer.Close();
                        _wpfMap.EditOverlay.CalculateAllControlPoints();
                        _wpfMap.Refresh();
                        break;
                }
            };
            ValidationRules.AddRange(new List<ValidationRule> { NorthValidationRule, SouthValidationRule, EastValidationRule, WestValidationRule });
        }

        void FeatureDraggedHandler(object sender, FeatureDraggedEditInteractiveOverlayEventArgs featureDraggedEditInteractiveOverlayEventArgs) 
        {
            _editOverlay.EditShapesLayer.Open();
            var bounds = _editOverlay.EditShapesLayer.GetBoundingBox();
            _editOverlay.EditShapesLayer.Close();
            North = bounds.UpperLeftPoint.Y;
            South = bounds.LowerRightPoint.Y;
            East = bounds.LowerRightPoint.X;
            West = bounds.UpperLeftPoint.X;
            Debug.WriteLine("Dragged: North {0} South {1} East {2} West {3}", bounds.UpperLeftPoint.Y, bounds.LowerRightPoint.Y, bounds.LowerRightPoint.X, bounds.UpperLeftPoint.X);
        }

        public void FeatureResizedHandler(object sender, FeatureResizedEditInteractiveOverlayEventArgs args)
        {
            _editOverlay.EditShapesLayer.Open();
            var bounds = _editOverlay.EditShapesLayer.GetBoundingBox();
            _editOverlay.EditShapesLayer.Close();
            North = bounds.UpperLeftPoint.Y;
            South = bounds.LowerRightPoint.Y;
            East = bounds.LowerRightPoint.X;
            West = bounds.UpperLeftPoint.X;
            Debug.WriteLine("Resized: North {0} South {1} East {2} West {3}", bounds.UpperLeftPoint.Y, bounds.LowerRightPoint.Y, bounds.LowerRightPoint.X, bounds.UpperLeftPoint.X);
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
                    _wpfMap.EditOverlay = null;
                    _editOverlay = null;
                    return;
                }
                _editOverlay = new CustomEditInteractiveOverlay();
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

        [Initialize( 10.0)] public double North { get; set; }
        [Initialize(-10.0)] public double South { get; set; }
        [Initialize( 10.0)] public double East { get; set; }
        [Initialize(-10.0)] public double West { get; set; }
        public GeoRect GeoRect { get { return new GeoRect(North, South, East, West); } }

        #region Validation Rules
        static readonly ValidationRule NorthValidationRule = new ValidationRule
        {
            PropertyName = "North",
            Description = "Must be between -90 and +90 and be greater than South",
            RuleDelegate = (o, r) =>
            {
                var target = (EditOverlayViewModel)o;
                return target.North >= -90 && target.North <= 90 && target.North > target.South;
            },
        };
        static readonly ValidationRule SouthValidationRule = new ValidationRule
        {
            PropertyName = "South",
            Description = "Must be between -90 and +90 and be less than North",
            RuleDelegate = (sender, eventArgs) =>
            {
                var target = (EditOverlayViewModel)sender;
                return target.South >= -90 && target.South <= 90 && target.North > target.South;
            },
        };
        static readonly ValidationRule EastValidationRule = new ValidationRule
        {
            PropertyName = "East",
            Description = "Must be between -180 and +180 and be greater than West",
            RuleDelegate = (sender, eventArgs) =>
            {
                var target = (EditOverlayViewModel)sender;
                return target.East >= -180 && target.East <= 180 && target.East > target.West;
            },
        };
        static readonly ValidationRule WestValidationRule = new ValidationRule
        {
            PropertyName = "West",
            Description = "Must be between -180 and +180 and be less than East",
            RuleDelegate = (sender, eventArgs) =>
            {
                var target = (EditOverlayViewModel)sender;
                return target.West >= -180 && target.West <= 180 && target.East > target.West;
            },
        };
        #endregion
    }
}
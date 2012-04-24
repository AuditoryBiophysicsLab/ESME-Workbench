using System;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Media;
using ESME.Model;
using HRC.Aspects;
using HRC.Services;
using HRC.Utility;
using HRC.ViewModels;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESME.Mapping
{
    [Serializable]
    [NotifyPropertyChanged]
    public class MapLayerViewModel : ISupportValidation, IHaveAName
    {
        protected static readonly Random Random;
        static readonly Color[] Palette;
        static int _paletteIndex;
        #region Menu Initializers
        protected readonly MenuItemViewModelBase ColorMenu = new MenuItemViewModelBase
        {
            Header = "Colors & Lines",
        };

        protected readonly MenuItemViewModelBase LineColorMenu = new MenuItemViewModelBase
        {
            Header = "Line Color",
        };

        protected readonly LineWeightMenuItemViewModel SymbolSizeMenu = new LineWeightMenuItemViewModel
        {
            Header = "Symbol Size",
        };

        protected readonly MenuItemViewModelBase PointColorMenu = new MenuItemViewModelBase
        {
            Header = "Symbol Color",
        };

        protected readonly PointSymbolTypeMenuItemViewModel PointStyleMenu = new PointSymbolTypeMenuItemViewModel
        {
            Header = "Symbol Shape",
            Children = new List<MenuItemViewModelBase>
            {
                new PointSymbolTypeMenuItemViewModel
                {
                    Header = "Circle",
                    IsCheckable = true,
                    PointSymbolType = PointSymbolType.Circle,
                },
                new PointSymbolTypeMenuItemViewModel
                {
                    Header = "Square",
                    IsCheckable = true,
                    PointSymbolType = PointSymbolType.Square,
                },
                new PointSymbolTypeMenuItemViewModel
                {
                    Header = "Triangle",
                    IsCheckable = true,
                    PointSymbolType = PointSymbolType.Triangle,
                },
                new PointSymbolTypeMenuItemViewModel
                {
                    Header = "Diamond",
                    IsCheckable = true,
                    PointSymbolType = PointSymbolType.Diamond,
                },
                new PointSymbolTypeMenuItemViewModel
                {
                    Header = "Diamond 2",
                    IsCheckable = true,
                    PointSymbolType = PointSymbolType.Diamond2,
                },
                new PointSymbolTypeMenuItemViewModel
                {
                    Header = "Star",
                    IsCheckable = true,
                    PointSymbolType = PointSymbolType.Star,
                },
                new PointSymbolTypeMenuItemViewModel
                {
                    Header = "Star 2",
                    IsCheckable = true,
                    PointSymbolType = PointSymbolType.Star2,
                },
            },
        };

        protected readonly LineWeightMenuItemViewModel LineWeightMenu = new LineWeightMenuItemViewModel
        {
            Header = "Line Weight",
        };

        protected readonly MenuItemViewModelBase MoveDownMenu = new MenuItemViewModelBase
        {
            Header = "Move down",
        };

        protected readonly MenuItemViewModelBase MoveToBottomMenu = new MenuItemViewModelBase
        {
            Header = "Move to bottom",
        };

        protected readonly MenuItemViewModelBase MoveToTopMenu = new MenuItemViewModelBase
        {
            Header = "Move to top",
        };

        protected readonly MenuItemViewModelBase MoveUpMenu = new MenuItemViewModelBase
        {
            Header = "Move up",
        };

        protected readonly MenuItemViewModelBase OrderMenu = new MenuItemViewModelBase
        {
            Header = "Layer Order",
        };

        protected readonly MenuItemViewModelBase PropertiesMenu = new MenuItemViewModelBase
        {
            Header = "Properties...",
            Visibility = Visibility.Collapsed,
        };

        protected readonly MenuItemViewModelBase RemoveMenu = new MenuItemViewModelBase
        {
            Header = "Remove Layer",
        };

        #endregion

        #region Helpers
        protected static Color RandomColor
        {
            get
            {
                var color = Palette[_paletteIndex++];
                _paletteIndex %= Palette.Length;
                return color;
            }
        }

        public static AreaStyle CreateAreaStyle(Color pen, float width, Color brush)
        {
            if (brush == Colors.Transparent) return new AreaStyle(new GeoPen(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B), width));
            return new AreaStyle(new GeoPen(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B), width),
                                 new GeoSolidBrush(GeoColor.FromArgb(brush.A, brush.R, brush.G, brush.B)));
        }

        public static LineStyle CreateLineStyle(Color pen, float width) { return new LineStyle(new GeoPen(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B), width)); }
        public static PointStyle CreatePointStyle(PointSymbolType pointSymbolType, Color pen, int width)
        {
            return new PointStyle(pointSymbolType, new GeoSolidBrush(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B)),
                                  width);
        }


        #endregion

        public bool IsValid { get; private set; }

        #region public string ValidationErrorText { get; set; }

        string _validationErrorText;

        public string ValidationErrorText
        {
            get
            {
                Validate();
                return _validationErrorText;
            }
            protected set
            {
                if (_validationErrorText == value) return;
                _validationErrorText = value;
                IsValid = string.IsNullOrEmpty(_validationErrorText);
            }
        }
        #endregion

        static MapLayerViewModel()
        {
            Random = new Random();
            Palette = ExtensionMethods.CreateHSVPalette(60);
        }

        public MapLayerViewModel()
        {
            CanChangeLineWidth = true;
            IsValid = true;
            LayerOverlay = new LayerOverlay
            {
                TileType = TileType.SingleTile,
            };
            LineColorBrush = new SolidColorBrush(_lineColor);
            AreaColorBrush = new SolidColorBrush(_areaColor);
            AreaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
            LineStyle = CreateLineStyle(LineColor, LineWidth);

            // These settings are defaults, which are overriden as appropriate by specific layers
            CanBeRemoved = false;
            CanBeReordered = true;

            LineColorMenu.Command = new SimpleCommand<object, object>(obj => CanChangeLineColor, obj =>
            {
                var result = ColorPickerService.ShowDialog();
                if (!result.HasValue || !result.Value) return;
                LineColor = ColorPickerService.Color;
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
            });

            ContextMenu = new List<MenuItemViewModelBase>
            {
                OrderMenu,
                RemoveMenu,
                PropertiesMenu,
            };

            LineColorPickerMenu = new List<MenuItemViewModelBase>
            {
                LineColorMenu,
                LineWeightMenu,
            };

            OrderMenu.Children.Add(MoveToTopMenu);
            OrderMenu.Children.Add(MoveUpMenu);
            OrderMenu.Children.Add(MoveDownMenu);
            OrderMenu.Children.Add(MoveToBottomMenu);

            for (var lineWidth = 1.0f; lineWidth <= 5; lineWidth += 0.5f)
            {
                var width = lineWidth;
                LineWeightMenu.Children.Add(new LineWeightMenuItemViewModel
                {
                    Header = string.Format("{0:0.0}", lineWidth),
                    IsCheckable = true,
                    LineWidth = lineWidth,
                    Command = new SimpleCommand<object, object>(obj => CanChangeLineWidth, obj =>
                    {
                        LineWidth = width;
                        MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
                    }),
                });
            }

            for (var pointSize = 1; pointSize <= 10; pointSize++)
            {
                var size = pointSize;
                SymbolSizeMenu.Children.Add(new LineWeightMenuItemViewModel
                {
                    Header = string.Format("{0}", pointSize),
                    IsCheckable = true,
                    LineWidth = pointSize,
                    Command = new SimpleCommand<object, object>(obj => CanChangeLineWidth, obj =>
                    {
                        LineWidth = size;
                        MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
                    }),
                });
            }

            ColorMenu.Children.Add(LineColorMenu);
            ColorMenu.Children.Add(LineWeightMenu);

            CheckProperLineWidthMenu();
        }

        public MapLayerCollection MapLayers { get; set; }

        public static IHRCColorPickerService ColorPickerService { get; set; }
        public static GeoCollection<Overlay> MapOverlay { get; set; }

        public LayerOverlay LayerOverlay { get; set; }

        public virtual Visibility IsLineColorVisible
        {
            get
            {
                switch (LayerType)
                {
                    default:
                        return Visibility.Hidden;
                    case LayerType.BaseMap:
                    case LayerType.Shapefile:
                    case LayerType.SimArea:
                    case LayerType.OpArea:
                    case LayerType.Bathymetry:
                    case LayerType.Animal:
                    case LayerType.SoundSpeed:
                    case LayerType.BottomType:
                    case LayerType.WindSpeed:
                    case LayerType.Pressure:
                        return Visibility.Visible;
                }
            }
        }

        public Visibility IsAreaColorVisible
        {
            get
            {
                switch (LayerType)
                {
                    default:
                        return Visibility.Hidden;
                    case LayerType.BaseMap:
                    case LayerType.Shapefile:
                        return Visibility.Visible;
                }
            }
        }

        public bool IsFeatureLayer
        {
            get
            {
                switch (LayerType)
                {
                    default:
                        return false;
                    case LayerType.Animal:
                    case LayerType.SoundSpeed:
                    case LayerType.BottomType:
                    case LayerType.WindSpeed:
                        return true;
                }
            }
        }

        public string PointLineTooltip
        {
            get
            {
                if (IsFeatureLayer) return "Symbol: Right click to change symbol, size, and color";
                return "Line: Right click to change width and color ";
            }
        }

        public string ToolTip { get; set; }

        LayerType _layerType;
        public LayerType LayerType
        {
            get { return _layerType; }
            set
            {
                _layerType = value;

                LineOrPointPickerMenu = IsFeatureLayer ? PointShapePickerMenu : LineColorPickerMenu;
            }
        }

        bool _canBeReordered;
        public bool CanBeReordered
        {
            get { return _canBeReordered; }
            set
            {
                OrderMenu.Visibility = value ? Visibility.Visible : Visibility.Collapsed;
                _canBeReordered = value;
            }
        }

        bool _canBeRemoved;
        public bool CanBeRemoved
        {
            get { return _canBeRemoved; }
            set
            {
                RemoveMenu.Visibility = value ? Visibility.Visible : Visibility.Collapsed;
                _canBeRemoved = value;
            }
        }
        bool _canChangeLineColor;
        public bool CanChangeLineColor
        {
            get { return _canChangeLineColor; }
            set
            {
                _canChangeLineColor = value;
            }
        }

        public bool CanChangeAreaColor { get; set; }

        public bool CanChangeLineWidth { get; set; }

        public List<MenuItemViewModelBase> ContextMenu { get; set; }

        public List<MenuItemViewModelBase> LineColorPickerMenu { get; set; }

        public List<MenuItemViewModelBase> LineOrPointPickerMenu { get; set; }

        public List<MenuItemViewModelBase> PointShapePickerMenu { get; set; }

        bool _isEnabled = true;
        public bool IsEnabled
        {
            get { return _isEnabled; }
            set
            {
                _isEnabled = value;
                if (!_isEnabled) IsChecked = false;
            }
        }
        bool _isChecked;
        public bool IsChecked
        {
            get { return _isChecked; }
            set
            {
                _isChecked = value;
                if (LayerOverlay != null) MediatorMessage.Send(_isChecked ? MediatorMessage.ShowMapLayer : MediatorMessage.HideMapLayer, LayerOverlay);
            }
        }

        public Brush LineColorBrush { get; set; }

        public Brush AreaColorBrush { get; set; }

        #region ISupportValidation Members
        public virtual void Validate()
        {
            ValidationErrorText = null;
        }
        #endregion

        AreaStyle _areaStyle;
        public AreaStyle AreaStyle
        {
            get { return _areaStyle; }
            set
            {
                _areaStyle = value;
                if (_areaStyle == null) return;

                _lineColor = Color.FromArgb(value.OutlinePen.Color.AlphaComponent, value.OutlinePen.Color.RedComponent,
                                           value.OutlinePen.Color.GreenComponent, value.OutlinePen.Color.BlueComponent);
                _lineWidth = value.OutlinePen.Width;
                _areaColor = Color.FromArgb(value.FillSolidBrush.Color.AlphaComponent,
                                           value.FillSolidBrush.Color.RedComponent,
                                           value.FillSolidBrush.Color.GreenComponent,
                                           value.FillSolidBrush.Color.BlueComponent);
                if (LayerOverlay.Layers.Count == 0) return;
                ((FeatureLayer)LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = _areaStyle;
                ((FeatureLayer)LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel =
                    ApplyUntilZoomLevel.Level20;
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
            }
        }

        LineStyle _lineStyle;
        public LineStyle LineStyle
        {
            get { return _lineStyle; }
            set
            {
                _lineStyle = value;
                if (_lineStyle == null) return;
                if (LayerOverlay.Layers.Count == 0) return;
                ((FeatureLayer)LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultLineStyle = _lineStyle;
                ((FeatureLayer)LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel =
                    ApplyUntilZoomLevel.Level20;
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
            }
        }

        #region public LineStyle CustomLineStyle { get; set; }
        LineStyle _customLineStyle;

        public LineStyle CustomLineStyle
        {
            get { return _customLineStyle; }
            set
            {
                _customLineStyle = value;
                if (_customLineStyle == null) return;
                if (LayerOverlay.Layers.Count == 0) return;
                ((FeatureLayer)LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.CustomStyles.Add(_customLineStyle);
                ((FeatureLayer)LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel =
                    ApplyUntilZoomLevel.Level20;
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
            }
        }
        #endregion

        #region public PointStyle PointStyle { get; set; }
        PointStyle _pointStyle;

        public PointStyle PointStyle
        {
            get { return _pointStyle; }
            set
            {
                _pointStyle = value;
                if (_pointStyle == null) return;
                if (LayerOverlay.Layers.Count == 0) return;
                ((FeatureLayer)LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultPointStyle = _pointStyle;
                ((FeatureLayer)LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel =
                    ApplyUntilZoomLevel.Level20;
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
            }
        }
        #endregion

        #region public float LineWidth { get; set; }
        float _lineWidth = Random.Next(2, 10) / 2.0f;
        public float LineWidth
        {
            get { return _lineWidth; }
            set
            {
                _lineWidth = value;
                _areaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
                _lineStyle = CreateLineStyle(LineColor, LineWidth);
                _pointStyle = CreatePointStyle(PointSymbolType, LineColor, (int)LineWidth);
                CheckProperLineWidthMenu();
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
            }
        }

        void CheckProperLineWidthMenu()
        {
            foreach (var child in LineWeightMenu.Children) child.IsChecked = ((LineWeightMenuItemViewModel)child).LineWidth == _lineWidth;
            foreach (var child in SymbolSizeMenu.Children) child.IsChecked = ((LineWeightMenuItemViewModel)child).LineWidth == _lineWidth;
        }
        #endregion

        Color _areaColor = RandomColor;
        public Color AreaColor
        {
            get { return _areaColor; }
            set
            {
                _areaColor = value;
                _areaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
                AreaColorBrush = new SolidColorBrush(_areaColor);
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
            }
        }

        public string Name
        {
            get { return LayerOverlay.Name; }
            set { LayerOverlay.Name = value; }
        }

        Color _lineColor = RandomColor;
        public Color LineColor
        {
            get { return _lineColor; }
            set
            {
                if (_lineColor == value) return;
                _lineColor = value;
                _areaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
                _lineStyle = CreateLineStyle(LineColor, LineWidth);
                _pointStyle = CreatePointStyle(PointSymbolType, LineColor, (int)Math.Max(1, LineWidth));
                LineColorBrush = new SolidColorBrush(_lineColor);
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
            }
        }

        #region public PointSymbolType PointSymbolType { get; set; }

        PointSymbolType _pointSymbolType = (PointSymbolType)(Random.Next(8));
        public PointSymbolType PointSymbolType
        {
            get { return _pointSymbolType; }
            set
            {
                _pointSymbolType = value;
                _pointStyle = CreatePointStyle(PointSymbolType, LineColor, (int)LineWidth);
                CheckProperPointSymbolTypeMenu();
            }
        }

        protected void CheckProperPointSymbolTypeMenu()
        {
            foreach (var child in PointStyleMenu.Children)
            {
                var typeSafeChild = (PointSymbolTypeMenuItemViewModel)child;
                child.IsChecked = typeSafeChild.PointSymbolType == _pointSymbolType;
            }
        }

        #endregion
    }
}
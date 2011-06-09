using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Windows;
using System.Windows.Media;
using System.Xml.Serialization;
using Cinch;
using ESME;
using ESME.Model;
using ESME.TransmissionLoss;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.Main;
using HRC.Services;
using HRC.Utility;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Map
{
    public class MapLayerViewModel : PropertyChangedBase, ISupportValidation 
    {
        static readonly Random Random;
        static readonly Color[] Palette;
        static int _paletteIndex;

        public static ObservableCollection<MapLayerViewModel> Layers;

        #region Menu Initializers
        readonly MenuItemViewModel _areaColorMenu = new MenuItemViewModel
                                                    {
                                                        Header = "Area Color",
                                                    };

        readonly MenuItemViewModel _colorMenu = new MenuItemViewModel
                                                {
                                                    Header = "Colors & Lines",
                                                };

        readonly MenuItemViewModel _lineColorMenu = new MenuItemViewModel
                                                    {
                                                        Header = "Line Color",
                                                    };

        readonly MenuItemViewModel _lineWeightMenu = new MenuItemViewModel
                                                     {
                                                         Header = "Line Weight",
                                                     };

        readonly MenuItemViewModel _moveDownMenu = new MenuItemViewModel
                                                   {
                                                       Header = "Move down",
                                                   };

        readonly MenuItemViewModel _moveToBottomMenu = new MenuItemViewModel
                                                       {
                                                           Header = "Move to bottom",
                                                       };

        readonly MenuItemViewModel _moveToTopMenu = new MenuItemViewModel
                                                    {
                                                        Header = "Move to top",
                                                    };

        readonly MenuItemViewModel _moveUpMenu = new MenuItemViewModel
                                                 {
                                                     Header = "Move up",
                                                 };

        readonly MenuItemViewModel _orderMenu = new MenuItemViewModel
                                                {
                                                    Header = "Layer Order",
                                                };

        readonly MenuItemViewModel _pointColorMenu = new MenuItemViewModel
                                                     {
                                                         Header = "Symbol Color",
                                                     };

        readonly MenuItemViewModel _pointStyleMenu = new MenuItemViewModel
                                                     {
                                                         Header = "Symbol Shape",
                                                         Children = new List<MenuItemViewModel>
                                                                    {
                                                                        new MenuItemViewModel
                                                                        {
                                                                            Header = "Circle",
                                                                            IsCheckable = true,
                                                                            PointSymbolType = PointSymbolType.Circle,
                                                                        },
                                                                        new MenuItemViewModel
                                                                        {
                                                                            Header = "Square",
                                                                            IsCheckable = true,
                                                                            PointSymbolType = PointSymbolType.Square,
                                                                        },
                                                                        new MenuItemViewModel
                                                                        {
                                                                            Header = "Triangle",
                                                                            IsCheckable = true,
                                                                            PointSymbolType = PointSymbolType.Triangle,
                                                                        },
                                                                        new MenuItemViewModel
                                                                        {
                                                                            Header = "Diamond",
                                                                            IsCheckable = true,
                                                                            PointSymbolType = PointSymbolType.Diamond,
                                                                        },
                                                                        new MenuItemViewModel
                                                                        {
                                                                            Header = "Diamond 2",
                                                                            IsCheckable = true,
                                                                            PointSymbolType = PointSymbolType.Diamond2,
                                                                        },
                                                                        new MenuItemViewModel
                                                                        {
                                                                            Header = "Star",
                                                                            IsCheckable = true,
                                                                            PointSymbolType = PointSymbolType.Star,
                                                                        },
                                                                        new MenuItemViewModel
                                                                        {
                                                                            Header = "Star 2",
                                                                            IsCheckable = true,
                                                                            PointSymbolType = PointSymbolType.Star2,
                                                                        },
                                                                    },
                                                     };

        readonly MenuItemViewModel _removeMenu = new MenuItemViewModel
                                                 {
                                                     Header = "Remove Layer",
                                                 };

        readonly MenuItemViewModel _settingsMenu = new MenuItemViewModel
                                                   {
                                                       Header = "Settings...",
                                                   };

        readonly MenuItemViewModel _symbolSizeMenu = new MenuItemViewModel
                                                     {
                                                         Header = "Symbol Size",
                                                     };
        #endregion

        Brush _areaColorBrush;
        int _index = -1;
        LayerOverlay _layerOverlay;

        Brush _lineColorBrush;

        #region Helpers

        [XmlIgnore]
        static Color RandomColor
        {
            get
            {
#if true
                var color = Palette[_paletteIndex++];
                _paletteIndex %= Palette.Length;
                return color;
#else
                if (_random == null) _random = new Random();
                var randomBytes = new byte[3];
                while (true)
                {
                    _random.NextBytes(randomBytes);
                    if ((randomBytes[0] >= 0x40) || (randomBytes[1] >= 0x40) || (randomBytes[2] >= 0x40)) break;
                }
                return Color.FromArgb(255, randomBytes[0], randomBytes[1], randomBytes[2]);
#endif
            }
        }

        static AreaStyle CreateAreaStyle(Color pen, float width, Color brush) { return new AreaStyle(new GeoPen(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B), width), new GeoSolidBrush(GeoColor.FromArgb(brush.A, brush.R, brush.G, brush.B))); }

        static LineStyle CreateLineStyle(Color pen, float width) { return new LineStyle(new GeoPen(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B), width)); }

        static PointStyle CreatePointStyle(PointSymbolType pointSymbolType, Color pen, int width) { return new PointStyle(pointSymbolType, new GeoSolidBrush(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B)), width); }

        #endregion

        #region Properties for analysis point layers only

        #region public Visibility VisibleIfAnalysisPointLayer { get; set; }
        
        [XmlIgnore]
        public Visibility VisibleIfAnalysisPointLayer
        {
            get { return LayerType == LayerType.AnalysisPoint ? Visibility.Visible : Visibility.Hidden; }
        }

        #endregion

        #region public string AnalysisPointCompletionTooltip { get; set; }

        [XmlIgnore]
        public string AnalysisPointCompletionTooltip
        {
            get { return _analysisPointCompletionTooltip; }
            set
            {
                if (_analysisPointCompletionTooltip == value) return;
                _analysisPointCompletionTooltip = value;
                NotifyPropertyChanged(AnalysisPointCompletionTooltipChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AnalysisPointCompletionTooltipChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.AnalysisPointCompletionTooltip);
        string _analysisPointCompletionTooltip = "Not yet calculated";

        #endregion

        #region public int PercentComplete { get; set; }

        [XmlIgnore]
        public int PercentComplete
        {
            get { return _percentComplete; }
            set
            {
                string toolTip;
                if (_percentComplete == value) return;
                _percentComplete = value;
                NotifyPropertyChanged(PercentCompleteChangedEventArgs);
                if (_percentComplete <= 0)
                {
                    toolTip = "Not yet calculated";
                    System.Diagnostics.Debug.WriteLine("12px-red-circle.png");
                }
                else
                {
                    toolTip = String.Format("{0}% complete", _percentComplete);
                    if ((_percentComplete > 0) && (_percentComplete <= 25))
                        System.Diagnostics.Debug.WriteLine("12px-yellow-circle-25pct.png");
                    else if ((_percentComplete > 25) && (_percentComplete <= 50))
                        System.Diagnostics.Debug.WriteLine("12px-yellow-circle-50pct.png");
                    else if ((_percentComplete > 50) && (_percentComplete <= 75))
                        System.Diagnostics.Debug.WriteLine("12px-yellow-circle-75pct.png");
                    else if (_percentComplete >= 100)
                    {
                        toolTip = "Calculation complete";
                        System.Diagnostics.Debug.WriteLine("12px-green-circle.png");
                    }
                }
                AnalysisPointCompletionTooltip = toolTip;
            }
        }

        static readonly PropertyChangedEventArgs PercentCompleteChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.PercentComplete);
        int _percentComplete;

        #endregion

        #endregion

        #region public bool IsValid { get; set; }

        [XmlIgnore]
        public bool IsValid
        {
            get
            {
                return _isValid;
            }
            private set
            {
                if (_isValid == value) return;
                _isValid = value;
                NotifyPropertyChanged(IsValidChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsValidChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.IsValid);
        bool _isValid = true;

        #endregion

        #region public string ValidationErrorText { get; set; }
        [XmlIgnore]
        public string ValidationErrorText
        {
            get
            {
                Validate();
                return _validationErrorText;
            }
            private set
            {
                if (_validationErrorText == value) return;
                _validationErrorText = value;
                IsValid = string.IsNullOrEmpty(_validationErrorText);
                NotifyPropertyChanged(ValidationErrorTextChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ValidationErrorTextChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.ValidationErrorText);
        string _validationErrorText;

        #endregion

        public void Validate()
        {
            if (LayerType != LayerType.AnalysisPoint)
            {
                ValidationErrorText = null;
                return;
            }
            if (AnalysisPoint == null)
            {
                ValidationErrorText = "Unable to validate - AnalysisPoint is null";
                return;
            }
            AnalysisPoint.Validate();
            ValidationErrorText = AnalysisPoint.ValidationErrorText;
        }

        #region public AreaStyle AreaStyle { get; set; }

        AreaStyle _areaStyle;

        [XmlIgnore]
        public AreaStyle AreaStyle
        {
            get { return _areaStyle; }
            set
            {
                if (_areaStyle == value) return;
                LineColor = Color.FromArgb(value.OutlinePen.Color.AlphaComponent, value.OutlinePen.Color.RedComponent, value.OutlinePen.Color.GreenComponent, value.OutlinePen.Color.BlueComponent);
                LineWidth = value.OutlinePen.Width;
                AreaColor = Color.FromArgb(value.FillSolidBrush.Color.AlphaComponent, value.FillSolidBrush.Color.RedComponent, value.FillSolidBrush.Color.GreenComponent, value.FillSolidBrush.Color.BlueComponent);
                _areaStyle = value;
                if (LayerOverlay.Layers.Count == 0) return;
                ((FeatureLayer) LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = _areaStyle;
                ((FeatureLayer) LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            }
        }

        #endregion

        #region public LineStyle LineStyle { get; set; }

        LineStyle _lineStyle;

        [XmlIgnore]
        public LineStyle LineStyle
        {
            get { return _lineStyle; }
            set
            {
                if (_lineStyle == value) return;
                _lineStyle = value;
                if (LayerOverlay.Layers.Count == 0) return;
                ((FeatureLayer) LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultLineStyle = _lineStyle;
                ((FeatureLayer) LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            }
        }

        #endregion

        #region public PointSymbolType PointSymbolType { get; set; }

        PointSymbolType _pointSymbolType = (PointSymbolType) (Random.Next(8));

        public PointSymbolType PointSymbolType
        {
            get { return _pointSymbolType; }
            set
            {
                if (_pointSymbolType == value) return;
                _pointSymbolType = value;
                PointStyle = CreatePointStyle(PointSymbolType, LineColor, (int) LineWidth);
                CheckProperPointSymbolTypeMenu();
            }
        }

        void CheckProperPointSymbolTypeMenu()
        {
            foreach (var child in _pointStyleMenu.Children)
            {
                child.IsChecked = child.PointSymbolType == _pointSymbolType;
            }
        }

        #endregion

        #region public LineStyle CustomLineStyle { get; set; }

        LineStyle _customLineStyle;

        [XmlIgnore]
        public LineStyle CustomLineStyle
        {
            get { return _customLineStyle; }
            set
            {
                if (_customLineStyle == value) return;
                _customLineStyle = value;
                if (LayerOverlay.Layers.Count == 0) return;
                ((FeatureLayer) LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.CustomStyles.Add(_customLineStyle);
                ((FeatureLayer) LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            }
        }

        #endregion

        #region public PointStyle PointStyle { get; set; }

        PointStyle _pointStyle;

        [XmlIgnore]
        public PointStyle PointStyle
        {
            get { return _pointStyle; }
            set
            {
                if (_pointStyle == value) return;
                _pointStyle = value;
                if (LayerOverlay.Layers.Count == 0) return;
                ((FeatureLayer) LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultPointStyle = _pointStyle;
                ((FeatureLayer) LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            }
        }

        #endregion

        #region public float LineWidth { get; set; }

        //float _lineWidth = (float)Math.Max(1, ((_random.NextDouble() * 9) + 1) / 2);
        float _lineWidth = Random.Next(2, 10) / 2.0f;

        [XmlElement]
        public float LineWidth
        {
            get { return _lineWidth; }
            set
            {
                if (_lineWidth == value) return;
                _lineWidth = value;
                AreaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
                LineStyle = CreateLineStyle(LineColor, LineWidth);
                PointStyle = CreatePointStyle(PointSymbolType, LineColor, (int) LineWidth);
                CheckProperLineWidthMenu();
            }
        }

        void CheckProperLineWidthMenu()
        {
            foreach (var child in _lineWeightMenu.Children) child.IsChecked = child.LineWidth == _lineWidth;
            foreach (var child in _symbolSizeMenu.Children) child.IsChecked = child.LineWidth == _lineWidth;
        }

        #endregion

        #region public Color AreaColor { get; set; }

        [XmlElement]
        public Color AreaColor
        {
            get { return _areaColor; }
            set
            {
                if (_areaColor == value) return;
                _areaColor = value;
                AreaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
                AreaColorBrush = new SolidColorBrush(_areaColor);
            }
        }

        static readonly PropertyChangedEventArgs AreaColorBrushChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.AreaColorBrush);
        Color _areaColor = RandomColor;

        #endregion

        #region public string Name { get; set; }

        [XmlElement]
        public string Name
        {
            get { return Overlay.Name; }
            set
            {
                if (Overlay.Name == value) return;
                Overlay.Name = value;
            }
        }

        #endregion

        #region public Color LineColor { get; set; }

        [XmlElement]
        public Color LineColor
        {
            get { return _lineColor; }
            set
            {
                if (_lineColor == value) return;
                _lineColor = value;
                AreaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
                LineStyle = CreateLineStyle(LineColor, LineWidth);
                PointStyle = CreatePointStyle(PointSymbolType, LineColor, (int) Math.Max(1, LineWidth));
                LineColorBrush = new SolidColorBrush(_lineColor);
            }
        }

        static readonly PropertyChangedEventArgs LineColorBrushChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.LineColorBrush);
        Color _lineColor = RandomColor;

        #endregion

        #region public AnalysisPoint AnalysisPoint { get; set; }

        [XmlIgnore]
        public AnalysisPoint AnalysisPoint
        {
            get { return _analysisPoint; }
            set
            {
                if (_analysisPoint == value) return;
                if ((value != null) && (_analysisPoint != null)) _analysisPoint.PropertyChanged -= AnalysisPointChanged;
                _analysisPoint = value;
                NotifyPropertyChanged(AnalysisPointChangedEventArgs);
                if (_analysisPoint != null) _analysisPoint.PropertyChanged += AnalysisPointChanged;
            }
        }

        void AnalysisPointChanged(object sender, PropertyChangedEventArgs e)
        {
            var ap = (AnalysisPoint) sender;
            ap.Validate();
            ValidationErrorText = ap.ValidationErrorText;
        }

        static readonly PropertyChangedEventArgs AnalysisPointChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.AnalysisPoint);
        AnalysisPoint _analysisPoint;

        #endregion

        static MapLayerViewModel()
        {
            Random = new Random();
            Palette = ExtensionMethods.CreateHSVPalette(60);
        }


        public MapLayerViewModel()
        {
            LayerOverlay = new LayerOverlay
                           {
                               TileType = TileType.SingleTile,
                           };
            LineColorBrush = new SolidColorBrush(_lineColor);
            AreaColorBrush = new SolidColorBrush(_areaColor);
            AreaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
            LineStyle = CreateLineStyle(LineColor, LineWidth);
            PointStyle = CreatePointStyle(PointSymbolType, LineColor, (int) LineWidth);

            // These settings are defaults, which are overriden as appropriate by specific layers
            HasSettings = false;
            CanBeRemoved = false;
            CanBeReordered = true;

            _removeMenu.Command = new SimpleCommand<object, object>(obj => CanBeRemoved, obj => MediatorMessage.Send(MediatorMessage.RemoveLayer, this));

            _settingsMenu.Command = new SimpleCommand<object, object>(obj => HasSettings, obj => MediatorMessage.Send(MediatorMessage.EditAnalysisPoint, AnalysisPoint));

            _lineColorMenu.Command = new SimpleCommand<object, object>(obj => CanChangeLineColor, obj =>
                                                                                                  {
                                                                                                      var result = ColorPickerService.ShowDialog();
                                                                                                      if (!result.HasValue || !result.Value) return;
                                                                                                      LineColor = ColorPickerService.Color;
                                                                                                      MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                                                      MediatorMessage.Send(MediatorMessage.RefreshLayer, this);
                                                                                                  });

            _areaColorMenu.Command = new SimpleCommand<object, object>(obj => CanChangeAreaColor, obj =>
                                                                                                  {
                                                                                                      var result = ColorPickerService.ShowDialog();
                                                                                                      if (!result.HasValue || !result.Value) return;
                                                                                                      AreaColor = ColorPickerService.Color;
                                                                                                      MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                                                      MediatorMessage.Send(MediatorMessage.RefreshLayer, this);
                                                                                                  });
            _pointColorMenu.Command = _lineColorMenu.Command;

            _moveToTopMenu.Command = new SimpleCommand<object, object>(arg => Index < (Layers.Count - 1), obj =>
                                                                                                          {
                                                                                                              MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                                                              MediatorMessage.Send(MediatorMessage.MoveLayerToTop, this);
                                                                                                          });
            _moveUpMenu.Command = new SimpleCommand<object, object>(arg => Index < (Layers.Count - 1), obj =>
                                                                                                       {
                                                                                                           MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                                                           MediatorMessage.Send(MediatorMessage.MoveLayerUp, this);
                                                                                                       });
            _moveDownMenu.Command = new SimpleCommand<object, object>(arg => Index > 0, obj =>
                                                                                        {
                                                                                            MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                                            MediatorMessage.Send(MediatorMessage.MoveLayerDown, this);
                                                                                        });
            _moveToBottomMenu.Command = new SimpleCommand<object, object>(arg => Index > 0, obj =>
                                                                                            {
                                                                                                MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                                                MediatorMessage.Send(MediatorMessage.MoveLayerToBottom, this);
                                                                                            });
            ContextMenu = new List<MenuItemViewModel>
                          {
                              _orderMenu,
                              _removeMenu,
                              _settingsMenu,
                          };

            LineColorPickerMenu = new List<MenuItemViewModel>
                                  {
                                      _lineColorMenu,
                                      _lineWeightMenu,
                                  };
            AreaColorPickerMenu = new List<MenuItemViewModel>
                                  {
                                      _areaColorMenu,
                                  };

            PointShapePickerMenu = new List<MenuItemViewModel>
                                   {
                                       _pointColorMenu,
                                       _symbolSizeMenu,
                                       _pointStyleMenu,
                                   };
            _orderMenu.Children.Add(_moveToTopMenu);
            _orderMenu.Children.Add(_moveUpMenu);
            _orderMenu.Children.Add(_moveDownMenu);
            _orderMenu.Children.Add(_moveToBottomMenu);

            for (var lineWidth = 1.0f; lineWidth <= 5; lineWidth += 0.5f)
            {
                var width = lineWidth;
                _lineWeightMenu.Children.Add(new MenuItemViewModel
                                             {
                                                 Header = string.Format("{0:0.0}", lineWidth),
                                                 IsCheckable = true,
                                                 LineWidth = lineWidth,
                                                 Command = new SimpleCommand<object, object>(obj => CanChangeLineWidth, obj =>
                                                                                                                        {
                                                                                                                            LineWidth = width;
                                                                                                                            MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                                                                            MediatorMessage.Send(MediatorMessage.RefreshLayer, this);
                                                                                                                        }),
                                             });
            }

            for (var pointSize = 1; pointSize <= 10; pointSize++)
            {
                var size = pointSize;
                _symbolSizeMenu.Children.Add(new MenuItemViewModel
                                             {
                                                 Header = string.Format("{0}", pointSize),
                                                 IsCheckable = true,
                                                 LineWidth = pointSize,
                                                 Command = new SimpleCommand<object, object>(obj => CanChangeLineWidth, obj =>
                                                                                                                        {
                                                                                                                            LineWidth = size;
                                                                                                                            MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                                                                            MediatorMessage.Send(MediatorMessage.RefreshLayer, this);
                                                                                                                        }),
                                             });
            }

            foreach (var item in _pointStyleMenu.Children)
            {
                var item1 = item;
                item.Command = new SimpleCommand<object, object>(obj =>
                                                                 {
                                                                     PointSymbolType = item1.PointSymbolType;
                                                                     MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                     MediatorMessage.Send(MediatorMessage.RefreshLayer, this);
                                                                 });
            }

            _colorMenu.Children.Add(_lineColorMenu);
            _colorMenu.Children.Add(_lineWeightMenu);
            _colorMenu.Children.Add(_areaColorMenu);

            IsChecked = true;


            CheckProperLineWidthMenu();
            CheckProperPointSymbolTypeMenu();
            while (PointSymbolType == PointSymbolType.Cross)
            {
                PointSymbolType = (PointSymbolType) (Random.Next(8));
            }
        }

        public static IHRCColorPickerService ColorPickerService { get; set; }
        public static GeoCollection<Overlay> MapOverlay { get; set; }

        [XmlIgnore]
        public LayerOverlay LayerOverlay
        {
            get { return _layerOverlay; }
            set
            {
                _layerOverlay = value;
                Overlay = _layerOverlay;
            }
        }

        [XmlIgnore]
        public Overlay Overlay { get; set; }

        #region public Brush LineColorBrush { get; set; }

        [XmlIgnore]
        public Brush LineColorBrush
        {
            get { return _lineColorBrush; }
            set
            {
                if (_lineColorBrush == value) return;
                _lineColorBrush = value;
                NotifyPropertyChanged(LineColorBrushChangedEventArgs);
            }
        }

        #endregion

        #region public Brush AreaColorBrush { get; set; }

        [XmlIgnore]
        public Brush AreaColorBrush
        {
            get { return _areaColorBrush; }
            set
            {
                if (_areaColorBrush == value) return;
                _areaColorBrush = value;
                NotifyPropertyChanged(AreaColorBrushChangedEventArgs);
            }
        }

        #endregion

        [XmlIgnore]
        public Visibility IsLineColorVisible
        {
            get
            {
                switch (LayerType)
                {
                    default:
                        return Visibility.Hidden;
                    case LayerType.BaseMap:
                    case LayerType.Shapefile:
                    case LayerType.OverlayFile:
                    case LayerType.SimArea:
                    case LayerType.OpArea:
                    case LayerType.Bathymetry:
                    case LayerType.Animal:
                    case LayerType.SoundSpeed:
                    case LayerType.BottomType:
                    case LayerType.WindSpeed:
                    case LayerType.AnalysisPoint:
                        return Visibility.Visible;
                }
            }
        }

        [XmlIgnore]
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

        [XmlIgnore]
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

        [XmlIgnore]
        public string PointLineTooltip
        {
            get
            {
                if (IsFeatureLayer) return "Symbol: Right click to change symbol, size, and color";
                return "Line: Right click to change width and color ";
            }
        }

        [XmlElement]
        public int Index
        {
            get { return (MapOverlay != null) ? MapOverlay.IndexOf(LayerOverlay) : _index; }
            set
            {
                if ((MapOverlay != null) && (MapOverlay.Contains(LayerOverlay))) MapOverlay.MoveTo(LayerOverlay, value);
                _index = value;
            }
        }

        #region public string ToolTip { get; set; }

        public string ToolTip
        {
            get { return _toolTip; }
            set
            {
                if (_toolTip == value) return;
                _toolTip = value;
                NotifyPropertyChanged(ToolTipChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs ToolTipChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.ToolTip);
        private string _toolTip;

        #endregion

        #region public LayerType LayerType { get; set; }

        LayerType _layerType;

        public LayerType LayerType
        {
            get { return _layerType; }
            set
            {
                if (_layerType == value) return;
                _layerType = value;

                LineOrPointPickerMenu = IsFeatureLayer ? PointShapePickerMenu : LineColorPickerMenu;
            }
        }

        #endregion

        #region public bool CanBeReordered { get; set; }

        static readonly PropertyChangedEventArgs CanBeReorderedChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.CanBeReordered);
        bool _canBeReordered;

        public bool CanBeReordered
        {
            get { return _canBeReordered; }
            set
            {
                _orderMenu.Visibility = value ? Visibility.Visible : Visibility.Collapsed;
                if (_canBeReordered == value) return;
                _canBeReordered = value;
                NotifyPropertyChanged(CanBeReorderedChangedEventArgs);
            }
        }

        #endregion

        #region public bool HasSettings { get; set; }

        public bool HasSettings
        {
            get { return _hasSettings; }
            set
            {
                _settingsMenu.Visibility = value ? Visibility.Visible : Visibility.Collapsed;
                if (_hasSettings == value) return;
                _hasSettings = value;
                NotifyPropertyChanged(HasSettingsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs HasSettingsChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.HasSettings);
        bool _hasSettings;

        #endregion

        #region public bool CanBeRemoved { get; set; }

        static readonly PropertyChangedEventArgs CanBeRemovedChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.CanBeRemoved);
        bool _canBeRemoved;

        public bool CanBeRemoved
        {
            get { return _canBeRemoved; }
            set
            {
                _removeMenu.Visibility = value ? Visibility.Visible : Visibility.Collapsed;
                if (_canBeRemoved == value) return;
                _canBeRemoved = value;
                NotifyPropertyChanged(CanBeRemovedChangedEventArgs);
            }
        }

        #endregion

        #region public bool CanChangeLineColor { get; set; }

        static readonly PropertyChangedEventArgs CanChangeLineColorChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.CanChangeLineColor);
        bool _canChangeLineColor;

        public bool CanChangeLineColor
        {
            get { return _canChangeLineColor; }
            set
            {
                if (_canChangeLineColor == value) return;
                _canChangeLineColor = value;
                NotifyPropertyChanged(CanChangeLineColorChangedEventArgs);
            }
        }

        #endregion

        #region public bool CanChangeAreaColor { get; set; }

        static readonly PropertyChangedEventArgs CanChangeAreaColorChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.CanChangeAreaColor);
        bool _canChangeAreaColor;

        public bool CanChangeAreaColor
        {
            get { return _canChangeAreaColor; }
            set
            {
                _canChangeAreaColor = value;
                NotifyPropertyChanged(CanChangeAreaColorChangedEventArgs);
            }
        }

        #endregion

        #region public bool CanChangeLineWidth { get; set; }

        static readonly PropertyChangedEventArgs CanChangeLineWidthChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.CanChangeLineWidth);
        bool _canChangeLineWidth = true;

        public bool CanChangeLineWidth
        {
            get { return _canChangeLineWidth; }
            set
            {
                if (_canChangeLineWidth == value) return;
                _canChangeLineWidth = value;
                NotifyPropertyChanged(CanChangeLineWidthChangedEventArgs);
            }
        }

        #endregion

        #region public List<MenuItemViewModel> ContextMenu { get; set; }

        static readonly PropertyChangedEventArgs ContextMenuChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.ContextMenu);
        List<MenuItemViewModel> _contextMenu;

        [XmlIgnore]
        public List<MenuItemViewModel> ContextMenu
        {
            get { return _contextMenu; }
            set
            {
                if (_contextMenu == value) return;
                _contextMenu = value;
                NotifyPropertyChanged(ContextMenuChangedEventArgs);
            }
        }

        #endregion

        #region public List<MenuItemViewModel> LineColorPickerMenu { get; set; }

        static readonly PropertyChangedEventArgs LineColorPickerMenuChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.LineColorPickerMenu);
        List<MenuItemViewModel> _lineColorPickerMenu;

        [XmlIgnore]
        public List<MenuItemViewModel> LineColorPickerMenu
        {
            get { return _lineColorPickerMenu; }
            set
            {
                if (_lineColorPickerMenu == value) return;
                _lineColorPickerMenu = value;
                NotifyPropertyChanged(LineColorPickerMenuChangedEventArgs);
            }
        }

        #endregion

        #region public List<MenuItemViewModel> LineOrPointPickerMenu { get; set; }

        static readonly PropertyChangedEventArgs LineOrPointPickerMenuChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.LineOrPointPickerMenu);
        List<MenuItemViewModel> _lineOrPointPickerMenu;

        [XmlIgnore]
        public List<MenuItemViewModel> LineOrPointPickerMenu
        {
            get { return _lineOrPointPickerMenu; }
            set
            {
                if (_lineOrPointPickerMenu == value) return;
                _lineOrPointPickerMenu = value;
                NotifyPropertyChanged(LineOrPointPickerMenuChangedEventArgs);
            }
        }

        #endregion

        #region public List<MenuItemViewModel> AreaColorPickerMenu { get; set; }

        static readonly PropertyChangedEventArgs AreaColorPickerMenuChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.AreaColorPickerMenu);
        List<MenuItemViewModel> _areaColorPickerMenu;

        [XmlIgnore]
        public List<MenuItemViewModel> AreaColorPickerMenu
        {
            get { return _areaColorPickerMenu; }
            set
            {
                if (_areaColorPickerMenu == value) return;
                _areaColorPickerMenu = value;
                NotifyPropertyChanged(AreaColorPickerMenuChangedEventArgs);
            }
        }

        #endregion

        #region public List<MenuItemViewModel> PointShapePickerMenu { get; set; }

        static readonly PropertyChangedEventArgs PointShapePickerMenuChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.PointShapePickerMenu);
        List<MenuItemViewModel> _pointShapePickerMenu;

        [XmlIgnore]
        public List<MenuItemViewModel> PointShapePickerMenu
        {
            get { return _pointShapePickerMenu; }
            set
            {
                if (_pointShapePickerMenu == value) return;
                _pointShapePickerMenu = value;
                NotifyPropertyChanged(PointShapePickerMenuChangedEventArgs);
            }
        }

        #endregion

        #region public bool IsChecked { get; set; }

        static readonly PropertyChangedEventArgs IsCheckedChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.IsChecked);
        bool _isChecked;

        public bool IsChecked
        {
            get { return _isChecked; }
            set
            {
                _isChecked = value;

                try
                {
                    LayerOverlay.IsVisible = _isChecked;
                }
                catch (NullReferenceException) {}

                MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                MediatorMessage.Send(MediatorMessage.RefreshLayer, this);

                NotifyPropertyChanged(IsCheckedChangedEventArgs);
            }
        }

        #endregion
    }
}
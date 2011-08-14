using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Windows;
using System.Windows.Media;
using System.Xml.Serialization;
using Cinch;
using ESME.Model;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.CASS;
using HRC.Services;
using HRC.Utility;
using HRC.ViewModels;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESME.Mapping
{
    public class MapLayerViewModel : PropertyChangedBase, ISupportValidation, IHaveAName
    {
        static readonly Random Random;
        static readonly Color[] Palette;
        static int _paletteIndex;

        #region Menu Initializers
        readonly MenuItemViewModelBase _areaColorMenu = new MenuItemViewModelBase
                                                    {
                                                        Header = "Area Color",
                                                    };

        readonly MenuItemViewModelBase _colorMenu = new MenuItemViewModelBase
                                                {
                                                    Header = "Colors & Lines",
                                                };

        readonly MenuItemViewModelBase _lineColorMenu = new MenuItemViewModelBase
                                                    {
                                                        Header = "Line Color",
                                                    };

        readonly LineWeightMenuItemViewModel _lineWeightMenu = new LineWeightMenuItemViewModel
                                                     {
                                                         Header = "Line Weight",
                                                     };

        readonly MenuItemViewModelBase _moveDownMenu = new MenuItemViewModelBase
                                                   {
                                                       Header = "Move down",
                                                   };

        readonly MenuItemViewModelBase _moveToBottomMenu = new MenuItemViewModelBase
                                                       {
                                                           Header = "Move to bottom",
                                                       };

        readonly MenuItemViewModelBase _moveToTopMenu = new MenuItemViewModelBase
                                                    {
                                                        Header = "Move to top",
                                                    };

        readonly MenuItemViewModelBase _moveUpMenu = new MenuItemViewModelBase
                                                 {
                                                     Header = "Move up",
                                                 };

        readonly MenuItemViewModelBase _orderMenu = new MenuItemViewModelBase
                                                {
                                                    Header = "Layer Order",
                                                };

        readonly MenuItemViewModelBase _pointColorMenu = new MenuItemViewModelBase
                                                     {
                                                         Header = "Symbol Color",
                                                     };

        readonly PointSymbolTypeMenuItemViewModel _pointStyleMenu = new PointSymbolTypeMenuItemViewModel
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

        readonly MenuItemViewModelBase _removeMenu = new MenuItemViewModelBase
        {
                Header = "Remove Layer",
        };

        readonly MenuItemViewModelBase _settingsMenu = new MenuItemViewModelBase
        {
                Header = "Settings...",
        };

        readonly LineWeightMenuItemViewModel _symbolSizeMenu = new LineWeightMenuItemViewModel
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

        #region public TreeNode TreeViewParent { get; set; }

        public TreeNode TreeViewParent
        {
            get { return _treeViewParent; }
            set
            {
                if (_treeViewParent == value) return;
                _treeViewParent = value;
                NotifyPropertyChanged(TreeViewParentChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TreeViewParentChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.TreeViewParent);
        TreeNode _treeViewParent;

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
            switch (LayerType)
            {
                case LayerType.AnalysisPoint:
                    if (AnalysisPoint == null)
                    {
                        ValidationErrorText = "Unable to validate - AnalysisPoint is null";
                        return;
                    }
                    AnalysisPoint.Validate();
                    ValidationErrorText = AnalysisPoint.ValidationErrorText;
                    break;
                case LayerType.Propagation:
                    if (CASSOutput == null)
                    {
                        ValidationErrorText = "Unable to validate - CASSOutput is null";
                        return;
                    }
                    CASSOutput.Validate();
                    ValidationErrorText = CASSOutput.ValidationErrorText;
                    break;
                default:
                    ValidationErrorText = null;
                    break;
            }
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
                var typeSafeChild = (PointSymbolTypeMenuItemViewModel)child;
                child.IsChecked = typeSafeChild.PointSymbolType == _pointSymbolType;
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
            foreach (var child in _lineWeightMenu.Children) child.IsChecked = ((LineWeightMenuItemViewModel)child).LineWidth == _lineWidth;
            foreach (var child in _symbolSizeMenu.Children) child.IsChecked = ((LineWeightMenuItemViewModel)child).LineWidth == _lineWidth;
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

        #region public CASSOutput CASSOutput { get; set; }

        public CASSOutput CASSOutput
        {
            get { return _cassOutput; }
            set
            {
                if (_cassOutput == value) return;
                if ((value != null) && (_cassOutput != null)) _cassOutput.PropertyChanged -= CASSOutputChanged;
                _cassOutput = value;
                NotifyPropertyChanged(CASSOutputChangedEventArgs);
                if (_cassOutput != null) _cassOutput.PropertyChanged += CASSOutputChanged;
            }
        }

        void CASSOutputChanged(object sender, PropertyChangedEventArgs e)
        {
            var point = (CASSOutput)sender;
            point.Validate();
            ValidationErrorText = point.ValidationErrorText;
        }

        static readonly PropertyChangedEventArgs CASSOutputChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.CASSOutput);
        CASSOutput _cassOutput;

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

        public MapLayerCollection MapLayers { get; set; }

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

            _moveToTopMenu.Command = new SimpleCommand<object, object>(arg => Index < (MapLayers.Count - 1), obj =>
                                                                                                          {
                                                                                                              MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                                                              MediatorMessage.Send(MediatorMessage.MoveLayerToTop, this);
                                                                                                          });
            _moveUpMenu.Command = new SimpleCommand<object, object>(arg => Index < (MapLayers.Count - 1), obj =>
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
            ContextMenu = new List<MenuItemViewModelBase>
                          {
                              _orderMenu,
                              _removeMenu,
                              _settingsMenu,
                          };

            LineColorPickerMenu = new List<MenuItemViewModelBase>
                                  {
                                      _lineColorMenu,
                                      _lineWeightMenu,
                                  };
            AreaColorPickerMenu = new List<MenuItemViewModelBase>
                                  {
                                      _areaColorMenu,
                                  };
            

            PointShapePickerMenu = new List<MenuItemViewModelBase>
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
                _lineWeightMenu.Children.Add(new LineWeightMenuItemViewModel
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
                _symbolSizeMenu.Children.Add(new LineWeightMenuItemViewModel
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
                                                                     PointSymbolType = ((PointSymbolTypeMenuItemViewModel)item1).PointSymbolType;
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
                    case LayerType.Propagation:
                    case LayerType.Pressure:
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

        #region public List<MenuItemViewModelBase> ContextMenu { get; set; }

        static readonly PropertyChangedEventArgs ContextMenuChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.ContextMenu);
        List<MenuItemViewModelBase> _contextMenu;

        [XmlIgnore]
        public List<MenuItemViewModelBase> ContextMenu
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

        #region public List<MenuItemViewModelBase> LineColorPickerMenu { get; set; }

        static readonly PropertyChangedEventArgs LineColorPickerMenuChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.LineColorPickerMenu);
        List<MenuItemViewModelBase> _lineColorPickerMenu;

        [XmlIgnore]
        public List<MenuItemViewModelBase> LineColorPickerMenu
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

        #region public List<MenuItemViewModelBase> LineOrPointPickerMenu { get; set; }

        static readonly PropertyChangedEventArgs LineOrPointPickerMenuChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.LineOrPointPickerMenu);
        List<MenuItemViewModelBase> _lineOrPointPickerMenu;

        [XmlIgnore]
        public List<MenuItemViewModelBase> LineOrPointPickerMenu
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

        #region public List<MenuItemViewModelBase> AreaColorPickerMenu { get; set; }

        static readonly PropertyChangedEventArgs AreaColorPickerMenuChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.AreaColorPickerMenu);
        List<MenuItemViewModelBase> _areaColorPickerMenu;

        [XmlIgnore]
        public List<MenuItemViewModelBase> AreaColorPickerMenu
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

        #region public List<MenuItemViewModelBase> PointShapePickerMenu { get; set; }

        static readonly PropertyChangedEventArgs PointShapePickerMenuChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.PointShapePickerMenu);
        List<MenuItemViewModelBase> _pointShapePickerMenu;

        [XmlIgnore]
        public List<MenuItemViewModelBase> PointShapePickerMenu
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
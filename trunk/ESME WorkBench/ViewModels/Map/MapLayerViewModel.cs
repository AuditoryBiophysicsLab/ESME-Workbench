using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Windows;
using System.Windows.Media;
using System.Xml.Serialization;
using Cinch;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.Main;
using HRC.Services;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Map
{
    public class MapLayerViewModel : INotifyPropertyChanged
    {
        static Random _random;

        public static ObservableCollection<MapLayerViewModel> Layers;
        public static IHRCColorPickerService ColorPickerService { get; set; }

        static MapLayerViewModel() { _random = new Random(); }

        public MapLayerViewModel()
        {
            LayerOverlay = new LayerOverlay();
            LineColorBrush = new SolidColorBrush(_lineColor);
            AreaColorBrush = new SolidColorBrush(_areaColor);
            AreaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
            LineStyle = CreateLineStyle(LineColor, LineWidth);
            PointStyle = CreatePointStyle(PointSymbolType, LineColor, (int)LineWidth);

            _removeMenu.Command = new SimpleCommand<object, object>(obj => CanBeRemoved, obj => MediatorMessage.Send(MediatorMessage.RemoveLayer, this));

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
                              _removeMenu
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
            while (PointSymbolType == PointSymbolType.Cross )
            {
                PointSymbolType= (PointSymbolType)(_random.Next(8));
            }
        }

        public static GeoCollection<Overlay> MapOverlay { get; set; }

        LayerOverlay _layerOverlay;

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

        readonly MenuItemViewModel _orderMenu = new MenuItemViewModel
                                                {
                                                    Header = "Layer Order",
                                                };

        readonly MenuItemViewModel _colorMenu = new MenuItemViewModel
                                                {
                                                    Header = "Colors & Lines",
                                                };

        readonly MenuItemViewModel _removeMenu = new MenuItemViewModel
                                                 {
                                                     Header = "Remove Layer",
                                                 };

        readonly MenuItemViewModel _lineColorMenu = new MenuItemViewModel
                                                    {
                                                        Header = "Line Color",
                                                    };

        readonly MenuItemViewModel _pointColorMenu = new MenuItemViewModel
                                                     {
                                                         Header = "Symbol Color",
                                                     };

        readonly MenuItemViewModel _areaColorMenu = new MenuItemViewModel
                                                    {
                                                        Header = "Area Color",
                                                    };

        readonly MenuItemViewModel _lineWeightMenu = new MenuItemViewModel
                                                     {
                                                         Header = "Line Weight",
                                                     };

        readonly MenuItemViewModel _symbolSizeMenu = new MenuItemViewModel
                                                     {
                                                         Header = "Symbol Size",
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

        readonly MenuItemViewModel _moveToTopMenu = new MenuItemViewModel
                                                    {
                                                        Header = "Move to top",
                                                    };

        readonly MenuItemViewModel _moveUpMenu = new MenuItemViewModel
                                                 {
                                                     Header = "Move up",
                                                 };

        readonly MenuItemViewModel _moveDownMenu = new MenuItemViewModel
                                                   {
                                                       Header = "Move down",
                                                   };

        readonly MenuItemViewModel _moveToBottomMenu = new MenuItemViewModel
                                                       {
                                                           Header = "Move to bottom",
                                                       };

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
                PointStyle = CreatePointStyle(PointSymbolType, LineColor, (int)Math.Max(1, LineWidth));
                LineColorBrush = new SolidColorBrush(_lineColor);
            }
        }

        Color _lineColor = RandomColor;

        #endregion

        [XmlIgnore]
        #region public Brush LineColorBrush { get; set; }

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

        static readonly PropertyChangedEventArgs LineColorBrushChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.LineColorBrush);
        Brush _lineColorBrush;

        #endregion

        #region public float LineWidth { get; set; }

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
                PointStyle = CreatePointStyle(PointSymbolType, LineColor, (int)LineWidth);
                CheckProperLineWidthMenu();
            }
        }

        //float _lineWidth = (float)Math.Max(1, ((_random.NextDouble() * 9) + 1) / 2);
        float _lineWidth = _random.Next(2, 10) / 2.0f;

        void CheckProperLineWidthMenu()
        {
            foreach (var child in _lineWeightMenu.Children)
            {
                child.IsChecked = child.LineWidth == _lineWidth;
            }
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

        Color _areaColor = RandomColor;

        #endregion

        [XmlIgnore]
        #region public Brush AreaColorBrush { get; set; }

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

        static readonly PropertyChangedEventArgs AreaColorBrushChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.AreaColorBrush);
        Brush _areaColorBrush;

        #endregion

        #region public AreaStyle AreaStyle { get; set; }

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

        AreaStyle _areaStyle;


        #endregion

        #region public LineStyle LineStyle { get; set; }

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

        LineStyle _lineStyle;

        #endregion

        #region public PointSymbolType PointSymbolType { get; set; }

        public PointSymbolType PointSymbolType
        {
            get { return _pointSymbolType; }
            set
            {
                if (_pointSymbolType == value) return;
                _pointSymbolType = value;
                PointStyle = CreatePointStyle(PointSymbolType, LineColor, (int)LineWidth);
                CheckProperPointSymbolTypeMenu();
            }
        }

        PointSymbolType _pointSymbolType = (PointSymbolType)(_random.Next(8));
       
        void CheckProperPointSymbolTypeMenu()
        {
            foreach (var child in _pointStyleMenu.Children)
            {
                child.IsChecked = child.PointSymbolType == _pointSymbolType;
            }
        }

        #endregion
        
        #region public LineStyle CustomLineStyle { get; set; }

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

        LineStyle _customLineStyle;

        #endregion

        #region public PointStyle PointStyle { get; set; }

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

        PointStyle _pointStyle;

        #endregion
        [XmlIgnore]
        public Visibility IsLineColorVisible
        {
            get
            {
                switch(LayerType)
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
                switch(LayerType)
                {
                    default:
                        return false;
                    case LayerType.Animal:
                        return true;
                }
            }
        }


        #region public LayerType LayerType { get; set; }

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

        LayerType _layerType;

        #endregion

        #region public bool CanBeReordered { get; set; }

        public bool CanBeReordered
        {
            get { return _canBeReordered; }
            set
            {
                if (_canBeReordered == value) return;
                _canBeReordered = value;
                NotifyPropertyChanged(CanBeReorderedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CanBeReorderedChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.CanBeReordered);
        bool _canBeReordered;

        #endregion

        #region public bool CanBeRemoved { get; set; }

        public bool CanBeRemoved
        {
            get { return _canBeRemoved; }
            set
            {
                if (_canBeRemoved == value) return;
                _canBeRemoved = value;
                NotifyPropertyChanged(CanBeRemovedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CanBeRemovedChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.CanBeRemoved);
        bool _canBeRemoved;

        #endregion

        #region public bool CanChangeLineColor { get; set; }

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

        static readonly PropertyChangedEventArgs CanChangeLineColorChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.CanChangeLineColor);
        bool _canChangeLineColor;

        #endregion

        #region public bool CanChangeAreaColor { get; set; }

        public bool CanChangeAreaColor
        {
            get { return _canChangeAreaColor; }
            set
            {
                _canChangeAreaColor = value;
                NotifyPropertyChanged(CanChangeAreaColorChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CanChangeAreaColorChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.CanChangeAreaColor);
        bool _canChangeAreaColor;

        #endregion

        #region public bool CanChangeLineWidth { get; set; }

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

        static readonly PropertyChangedEventArgs CanChangeLineWidthChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.CanChangeLineWidth);
        bool _canChangeLineWidth = true;

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

        static readonly PropertyChangedEventArgs LineColorPickerMenuChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.LineColorPickerMenu);
        List<MenuItemViewModel> _lineColorPickerMenu;

        #endregion

        #region public List<MenuItemViewModel> LineOrPointPickerMenu { get; set; }
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

        static readonly PropertyChangedEventArgs LineOrPointPickerMenuChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.LineOrPointPickerMenu);
        List<MenuItemViewModel> _lineOrPointPickerMenu;

        #endregion

        #region public List<MenuItemViewModel> AreaColorPickerMenu { get; set; }
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

        static readonly PropertyChangedEventArgs AreaColorPickerMenuChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.AreaColorPickerMenu);
        List<MenuItemViewModel> _areaColorPickerMenu;

        #endregion
        
        #region public List<MenuItemViewModel> PointShapePickerMenu { get; set; }
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

        static readonly PropertyChangedEventArgs PointShapePickerMenuChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.PointShapePickerMenu);
        List<MenuItemViewModel> _pointShapePickerMenu;

        #endregion
        
        #region public bool IsChecked { get; set; }

        static readonly PropertyChangedEventArgs IsCheckedChangedEventArgs = ObservableHelper.CreateArgs<MapLayerViewModel>(x => x.IsChecked);
        bool _isChecked;

        public bool IsChecked
        {
            get { return _isChecked; }
            set
            {
                if (value == _isChecked) return;

                _isChecked = value;

                try
                {
                    LayerOverlay.IsVisible = _isChecked;
                }
                catch (NullReferenceException) {}

                NotifyPropertyChanged(IsCheckedChangedEventArgs);
            }
        }

        #endregion

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

        int _index = -1;

        #region Helpers

        static AreaStyle CreateAreaStyle(Color pen, float width, Color brush) { return new AreaStyle(new GeoPen(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B), width), new GeoSolidBrush(GeoColor.FromArgb(brush.A, brush.R, brush.G, brush.B))); }

        static LineStyle CreateLineStyle(Color pen, float width) { return new LineStyle(new GeoPen(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B), width)); }

        static PointStyle CreatePointStyle(PointSymbolType pointSymbolType, Color pen, int width) { return new PointStyle(pointSymbolType, new GeoSolidBrush(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B)), width); }

        [XmlIgnore]
        static Color RandomColor
        {
            get
            {
                if (_random == null) _random = new Random();
                var randomBytes = new byte[3];
                _random.NextBytes(randomBytes);
                return Color.FromArgb(255, randomBytes[0], randomBytes[1], randomBytes[2]);
            }
        }

        #endregion

        void NotifyPropertyChanged(PropertyChangedEventArgs e) { if (PropertyChanged != null) PropertyChanged(this, e); }
        public event PropertyChangedEventHandler PropertyChanged;
    }
}
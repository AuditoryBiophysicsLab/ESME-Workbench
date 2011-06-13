﻿using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Windows.Forms;
using System.Windows.Media;
using System.Xml.Serialization;
using Cinch;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.Main;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Map
{
    public class MapLayerViewModel : INotifyPropertyChanged
    {
        static Random _random;

        static readonly ColorDialog ColorDialog = new ColorDialog
                                                  {
                                                      AllowFullOpen = true,
                                                      AnyColor = true
                                                  };

        public static ObservableCollection<MapLayerViewModel> Layers;

        public MapLayerViewModel()
        {
            LayerOverlay = new LayerOverlay();

            _removeMenu.Command = new SimpleCommand<MapLayerViewModel, MapLayerViewModel>(obj => CanBeRemoved, obj => MediatorMessage.Send(MediatorMessage.RemoveLayer, this));

            _lineColorMenu.Command = new SimpleCommand<MapLayerViewModel, MapLayerViewModel>(obj => CanChangeLineColor, obj =>
                                                                                                                        {
                                                                                                                            var result = ColorDialog.ShowDialog();
                                                                                                                            if (result != DialogResult.OK) return;
                                                                                                                            LineColor = Color.FromArgb(ColorDialog.Color.A, ColorDialog.Color.R, ColorDialog.Color.G, ColorDialog.Color.B);
                                                                                                                            MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                                                                            MediatorMessage.Send(MediatorMessage.RefreshLayer, this);
                                                                                                                        });

            _areaColorMenu.Command = new SimpleCommand<MapLayerViewModel, MapLayerViewModel>(obj => CanChangeAreaColor, obj =>
                                                                                                                        {
                                                                                                                            var result = ColorDialog.ShowDialog();
                                                                                                                            if (result != DialogResult.OK) return;
                                                                                                                            AreaColor = Color.FromArgb(ColorDialog.Color.A, ColorDialog.Color.R, ColorDialog.Color.G, ColorDialog.Color.B);
                                                                                                                            MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                                                                            MediatorMessage.Send(MediatorMessage.RefreshLayer, this);
                                                                                                                        });

            _moveToTopMenu.Command = new SimpleCommand<MapLayerViewModel, MapLayerViewModel>(arg => Index < (Layers.Count - 1), obj =>
                                                                                                                                {
                                                                                                                                    MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                                                                                    MediatorMessage.Send(MediatorMessage.MoveLayerToTop, this);
                                                                                                                                });
            _moveUpMenu.Command = new SimpleCommand<MapLayerViewModel, MapLayerViewModel>(arg => Index < (Layers.Count - 1), obj =>
                                                                                                                             {
                                                                                                                                 MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                                                                                 MediatorMessage.Send(MediatorMessage.MoveLayerUp, this);
                                                                                                                             });
            _moveDownMenu.Command = new SimpleCommand<MapLayerViewModel, MapLayerViewModel>(arg => Index > 0, obj =>
                                                                                                              {
                                                                                                                  MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                                                                  MediatorMessage.Send(MediatorMessage.MoveLayerDown, this);
                                                                                                              });
            _moveToBottomMenu.Command = new SimpleCommand<MapLayerViewModel, MapLayerViewModel>(arg => Index > 0, obj =>
                                                                                                                  {
                                                                                                                      MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                                                                      MediatorMessage.Send(MediatorMessage.MoveLayerToBottom, this);
                                                                                                                  });

            ContextMenu = new List<MenuItemViewModel<MapLayerViewModel>>
                          {
                              _orderMenu,
                              _colorMenu,
                              _removeMenu
                          };
            _orderMenu.Children.Add(_moveToTopMenu);
            _orderMenu.Children.Add(_moveUpMenu);
            _orderMenu.Children.Add(_moveDownMenu);
            _orderMenu.Children.Add(_moveToBottomMenu);

            for (var lineWidth = 0.5f; lineWidth <= 5; lineWidth += 0.5f)
            {
                var width = lineWidth;
                _lineWeightMenu.Children.Add(new MenuItemViewModel<MapLayerViewModel>
                                             {
                                                 Header = string.Format("{0:0.0}", lineWidth),
                                                 Command = new SimpleCommand<MapLayerViewModel, MapLayerViewModel>(obj => CanChangeLineWidth, obj =>
                                                                                                                                              {
                                                                                                                                                  LineWidth = width;
                                                                                                                                                  MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                                                                                                                                                  MediatorMessage.Send(MediatorMessage.RefreshLayer, this);
                                                                                                                                              }),
                                             });
            }

            _colorMenu.Children.Add(_lineColorMenu);
            _colorMenu.Children.Add(_lineWeightMenu);
            _colorMenu.Children.Add(_areaColorMenu);
        }

        public static GeoCollection<Overlay> MapOverlay
        {
            set { _mapOverlay = value; }
        }

        static GeoCollection<Overlay> _mapOverlay;

        [XmlIgnore]
        public LayerOverlay LayerOverlay { get; set; }

        readonly MenuItemViewModel<MapLayerViewModel> _orderMenu = new MenuItemViewModel<MapLayerViewModel>
                                                                   {
                                                                       Header = "Layer Order",
                                                                   };

        readonly MenuItemViewModel<MapLayerViewModel> _colorMenu = new MenuItemViewModel<MapLayerViewModel>
                                                                   {
                                                                       Header = "Colors & Lines",
                                                                   };

        readonly MenuItemViewModel<MapLayerViewModel> _removeMenu = new MenuItemViewModel<MapLayerViewModel>
                                                                    {
                                                                        Header = "Remove Layer",
                                                                    };

        readonly MenuItemViewModel<MapLayerViewModel> _lineColorMenu = new MenuItemViewModel<MapLayerViewModel>
                                                                       {
                                                                           Header = "Line Color",
                                                                       };

        readonly MenuItemViewModel<MapLayerViewModel> _areaColorMenu = new MenuItemViewModel<MapLayerViewModel>
                                                                       {
                                                                           Header = "Area Color",
                                                                       };

        readonly MenuItemViewModel<MapLayerViewModel> _lineWeightMenu = new MenuItemViewModel<MapLayerViewModel>
                                                                        {
                                                                            Header = "Line Weight",
                                                                        };

        readonly MenuItemViewModel<MapLayerViewModel> _moveToTopMenu = new MenuItemViewModel<MapLayerViewModel>
                                                                       {
                                                                           Header = "Move to top",
                                                                       };

        readonly MenuItemViewModel<MapLayerViewModel> _moveUpMenu = new MenuItemViewModel<MapLayerViewModel>
                                                                    {
                                                                        Header = "Move up",
                                                                    };

        readonly MenuItemViewModel<MapLayerViewModel> _moveDownMenu = new MenuItemViewModel<MapLayerViewModel>
                                                                      {
                                                                          Header = "Move down",
                                                                      };

        readonly MenuItemViewModel<MapLayerViewModel> _moveToBottomMenu = new MenuItemViewModel<MapLayerViewModel>
                                                                          {
                                                                              Header = "Move to bottom",
                                                                          };

        #region public string Name { get; set; }

        [XmlElement]
        public string Name
        {
            get { return LayerOverlay.Name; }
            set
            {
                if (LayerOverlay.Name == value) return;
                LayerOverlay.Name = value;
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
            }
        }

        Color _lineColor = Color.FromArgb(0, 0, 0, 0);

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
            }
        }

        float _lineWidth = 1f;

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
            }
        }

        Color _areaColor = Color.FromArgb(0, 0, 0, 0);

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

        #region public LayerType LayerType { get; set; }

        public LayerType LayerType
        {
            get { return _layerType; }
            set
            {
                if (_layerType == value) return;
                _layerType = value;
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
        List<MenuItemViewModel<MapLayerViewModel>> _contextMenu;

        [XmlIgnore]
        public List<MenuItemViewModel<MapLayerViewModel>> ContextMenu
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
            get { return (_mapOverlay != null) ? _mapOverlay.IndexOf(LayerOverlay) : _index; }
            set
            {
                if ((_mapOverlay != null) && (_mapOverlay.Contains(LayerOverlay))) _mapOverlay.MoveTo(LayerOverlay, value);
                _index = value;
            }
        }

        int _index = -1;

        #region Helpers

        static AreaStyle CreateAreaStyle(Color pen, float width, Color brush) { return new AreaStyle(new GeoPen(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B), width), new GeoSolidBrush(GeoColor.FromArgb(brush.A, brush.R, brush.G, brush.B))); }

        static LineStyle CreateLineStyle(Color pen, float width) { return new LineStyle(new GeoPen(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B), width)); }

        [XmlIgnore]
        public static AreaStyle RandomAreaStyle
        {
            get { return CreateAreaStyle(RandomColor, 1f, RandomColor); }
        }

        [XmlIgnore]
        public static LineStyle RandomLineStyle
        {
            get { return CreateLineStyle(RandomColor, 1f); }
        }

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
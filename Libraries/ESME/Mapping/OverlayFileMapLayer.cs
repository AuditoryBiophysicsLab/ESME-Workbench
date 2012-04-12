using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Windows;
using System.Xml.Serialization;
using Cinch;
using ESME.NEMO.Overlay;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.CASS;
using HRC.ViewModels;
using ThinkGeo.MapSuite.Core;

namespace ESME.Mapping
{
    [Serializable]
    public class OverlayFileMapLayer : OverlayShapeMapLayer
    {
        public OverlayFileMapLayer()
        {
            LayerType = LayerType.OverlayFile;
            LayerOverlay.Layers.Clear();
        }

        #region public string OverlayFileName { get; set; }
        [XmlIgnore]
        string _overlayFileName;

        [XmlElement]
        public string OverlayFileName
        {
            get { return _overlayFileName; }
            set
            {
                if (_overlayFileName == value) return;
                _overlayFileName = value;
                //Name = Path.GetFileNameWithoutExtension(_overlayFileName);
                Clear();
                var overlayFile = new OverlayFile(_overlayFileName);
                Add(overlayFile.Shapes);
                Done();
            }
        }
        #endregion
    }

    [Serializable]
    public class OverlayShapeMapLayer : MapLayerViewModel
    {
        InMemoryFeatureLayer _layer;

        public OverlayShapeMapLayer()
        {
            LayerOverlay.Layers.Clear();
            PointColorMenu.Command = LineColorMenu.Command;
            PointShapePickerMenu = new List<MenuItemViewModelBase>
            {
                PointColorMenu,
                SymbolSizeMenu,
                PointStyleMenu,
            };
            PointStyle = CreatePointStyle(PointSymbolType, LineColor, (int)LineWidth);
            foreach (var item in PointStyleMenu.Children)
            {
                var item1 = item;
                item.Command = new SimpleCommand<object, object>(obj =>
                {
                    PointSymbolType = ((PointSymbolTypeMenuItemViewModel)item1).PointSymbolType;
                    MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                    MediatorMessage.Send(MediatorMessage.RefreshLayer, this);
                });
            }
            CheckProperPointSymbolTypeMenu();
            while (PointSymbolType == PointSymbolType.Cross) PointSymbolType = (PointSymbolType)(Random.Next(8));
        }

        public void Add(OverlayShape overlayShape)
        {
            if (_layer == null) _layer = new InMemoryFeatureLayer();
            if (CustomLineStyle == null)
            {
                if (LineStyle == null)
                {
                    LineColor = overlayShape.Color;
                    LineWidth = overlayShape.Width;
                }
                if (PointStyle == null)
                    PointStyle = new PointStyle(PointSymbolType.Circle,
                                                new GeoSolidBrush(GeoColor.FromArgb(LineColor.A, LineColor.R,
                                                                                    LineColor.G, LineColor.B)),
                                                (int)LineWidth);
            }
            var wellKnownText = overlayShape.WellKnownText;
            if (wellKnownText != null)
                _layer.InternalFeatures.Add(
                                            new Feature(
                                                BaseShape.CreateShapeFromWellKnownData(overlayShape.WellKnownText)));
        }

        public void Add(IEnumerable<OverlayShape> overlayShapes) { foreach (var shape in overlayShapes) Add(shape); }

        public void Clear() { if (_layer != null) _layer.InternalFeatures.Clear(); }

        public void Done()
        {
            if (_layer == null) return;
            if (CustomLineStyle != null) _layer.ZoomLevelSet.ZoomLevel01.CustomStyles.Add(CustomLineStyle);
            else
            {
                if (LineStyle != null) _layer.ZoomLevelSet.ZoomLevel01.DefaultLineStyle = LineStyle;
                if (PointStyle != null) _layer.ZoomLevelSet.ZoomLevel01.DefaultPointStyle = PointStyle;
            }

            _layer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            LayerOverlay.Layers.Add(_layer);
            MediatorMessage.Send(MediatorMessage.RefreshLayer, this);
        }

        [XmlIgnore]
        public override Visibility IsLineColorVisible
        {
            get { return Visibility.Visible; }
        }

    }

    [Serializable]
    public class AnalysisPointLayer : OverlayShapeMapLayer
    {
        public AnalysisPointLayer()
        {
            ContextMenu.Add(new MenuItemViewModelBase
            {
                Header = "Properties...",
                Command = new SimpleCommand<object, object>(obj => MediatorMessage.Send(MediatorMessage.EditAnalysisPoint, AnalysisPoint)),
            });
            LayerType = LayerType.AnalysisPoint;
            RemoveMenu.Command = new SimpleCommand<object, object>(obj => CanBeRemoved, obj =>
            {
                MediatorMessage.Send(MediatorMessage.RemoveLayer, this);
                if (AnalysisPoint != null) MediatorMessage.Send(MediatorMessage.RemoveAnalysisPoint, AnalysisPoint);
            });

        }

        static readonly PropertyChangedEventArgs AnalysisPointChangedEventArgs =
            ObservableHelper.CreateArgs<AnalysisPointLayer>(x => x.AnalysisPoint);

        AnalysisPoint _analysisPoint;

        [XmlIgnore]
        public AnalysisPoint AnalysisPoint
        {
            get { return _analysisPoint; }
            set
            {
                if (_analysisPoint == value) return;
                if ((value != null) && (_analysisPoint != null)) _analysisPoint.PropertyChanged -= AnalysisPointChanged;
                _analysisPoint = value;
                OnPropertyChanged(AnalysisPointChangedEventArgs);
                if (_analysisPoint != null) _analysisPoint.PropertyChanged += AnalysisPointChanged;
            }
        }

        void AnalysisPointChanged(object sender, PropertyChangedEventArgs e)
        {
            var ap = (AnalysisPoint)sender;
            ap.Validate();
            ValidationErrorText = ap.ValidationErrorText;
        }

        public override void Validate()
        {
            if (AnalysisPoint == null)
            {
                ValidationErrorText = "Unable to validate - AnalysisPoint is null";
                return;
            }
            AnalysisPoint.Validate();
            ValidationErrorText = AnalysisPoint.ValidationErrorText;
        }
    }

    [Serializable]
    public class PropagationLayer : OverlayShapeMapLayer
    {
        public PropagationLayer() 
        {
            LayerType = LayerType.Propagation;
            ContextMenu.Add(new MenuItemViewModelBase
            {
                Header = "View...",
                Command = new SimpleCommand<object, object>(obj => MediatorMessage.Send(MediatorMessage.ViewPropagation, _transmissionLoss)),
            });
        }

        Scenarios.TransmissionLoss _transmissionLoss;

        [XmlIgnore]
        public Scenarios.TransmissionLoss TransmissionLoss
        {
            get { return _transmissionLoss; }
            set
            {
                _transmissionLoss = value;
                Validate();
            }
        }

        public override void Validate()
        {
            if (TransmissionLoss == null)
            {
                ValidationErrorText = "Unable to validate - TransmissionLoss is null";
                return;
            }
            TransmissionLoss.Validate();
            ValidationErrorText = TransmissionLoss.ValidationErrorText;
        }
    }
}
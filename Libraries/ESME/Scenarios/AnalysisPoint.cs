﻿using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Text;
using System.Windows.Media;
using ESME.Database;
using ESME.Environment;
using ESME.Locations;
using ESME.Mapping;
using ESME.Model;
using ESME.TransmissionLoss.Bellhop;
using HRC.Aspects;
using HRC.Navigation;
using ThinkGeo.MapSuite.Core;

namespace ESME.Scenarios
{
    [NotifyPropertyChanged]
    public class AnalysisPoint : IHaveGuid, IHaveLayerSettings
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public DbGeo Geo { get; set; }

        public virtual Scenario Scenario { get; set; }
        public virtual LayerSettings LayerSettings { get; set; }

        public virtual ICollection<TransmissionLoss> TransmissionLosses { get; set; }

        public void CreateMapLayers() { throw new NotImplementedException(); }
    }

    [NotifyPropertyChanged]
    public class TransmissionLoss : IHaveGuid, IHaveLayerSettings, ISupportValidation
    {
        [Key, Initialize]
        public Guid Guid { get; set; }

        public bool IsReadyToCalculate { get; set; }
        public virtual AnalysisPoint AnalysisPoint { get; set; }
        public virtual Mode Mode { get; set; }
        public virtual LayerSettings LayerSettings { get; set; }

        public virtual ICollection<Radial> Radials { get; set; }

        [NotMapped]
        public string LayerName { get { return string.Format("[{0:0.###}, {1:0.###}]", AnalysisPoint.Geo.Latitude, AnalysisPoint.Geo.Longitude); } }

        public void Validate()
        {
            if (Scenario.Database == null || Scenario.Cache == null || AnalysisPoint == null || AnalysisPoint.Scenario.Bathymetry == null)
            {
                ValidationErrorText = "Unable to validate";
                return;
            }
            var geoRect = ((Bathymetry)Scenario.Cache[AnalysisPoint.Scenario.Bathymetry]).Samples.GeoRect;

            if (!geoRect.Contains(AnalysisPoint.Geo))
            {
                ValidationErrorText = "Propagation point not contained within bathymetry bounds";
                return;
            }
            var errors = new StringBuilder();

            foreach (var radial in Radials.Where(radial => !geoRect.Contains(radial.Segment[1]))) errors.AppendLine(string.Format("Radial with bearing {0} extends beyond bathymetry bounds", radial.Bearing));
            ValidationErrorText = errors.ToString().Trim();
        }

        [NotMapped]
        public bool IsValid { get { return string.IsNullOrEmpty(ValidationErrorText); } }

        [Affects("IsValid")]
        [NotMapped]
        public string ValidationErrorText
        {
            get { Validate(); return _validationErrorText; }
            private set { _validationErrorText = value; }
        }
        string _validationErrorText;

        OverlayShapeMapLayer _mapLayer;
        public void CreateMapLayers()
        {
            _mapLayer = new OverlayShapeMapLayer
            {
                LayerType = LayerType.Track,
                Name = string.Format("{0}", Guid),
            };
            var geos = new List<Geo>();
            var startGeo = AnalysisPoint.Geo;
            foreach (var radial in Radials)
            {
                geos.Add(startGeo);
                geos.Add(((Geo)startGeo).Offset(Geo.KilometersToRadians(radial.Length / 1000), Geo.DegreesToRadians(radial.Bearing)));
                geos.Add(startGeo);
            }
            _mapLayer.Add(geos);
            _mapLayer.Done();

            LayerSettings.PropertyChanged += (s, e) =>
            {
                switch (e.PropertyName)
                {
                    case "IsChecked":
                        MediatorMessage.Send(LayerSettings.IsChecked ? MediatorMessage.ShowMapLayer : MediatorMessage.HideMapLayer, _mapLayer);
                        break;
                    case "LineOrSymbolColor":
                        _mapLayer.LineColor = LayerSettings.LineOrSymbolColor;
                        MediatorMessage.Send(MediatorMessage.RefreshMapLayer, _mapLayer);
                        break;
                    case "LineOrSymbolSize":
                        _mapLayer.LineWidth = (float)LayerSettings.LineOrSymbolSize;
                        MediatorMessage.Send(MediatorMessage.RefreshMapLayer, _mapLayer);
                        break;
                }
            };
        }
    }

    [NotifyPropertyChanged]
    public class Radial : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public bool IsCalculated { get; set; }
        public string Filename { get; set; }
        public DbDateTime CalculationStarted { get; set; }
        public DbDateTime CalculationCompleted { get; set; }
        /// <summary>
        /// In degrees, clockwise from true north
        /// </summary>
        public double Bearing { get; set; }
        /// <summary>
        /// In meters
        /// </summary>
        public double Length { get; set; }
        public byte[] RangeAxisBlob { get; set; }
        public byte[] DepthAxisBlob { get; set; }
        public byte[] BottomProfileBlob { get; set; }
        public byte[] MinimumTransmissionLossBlob { get; set; }
        public byte[] MaximumTransmissionLossBlob { get; set; }
        public byte[] MeanTransmissionLossBlob { get; set; }

        public virtual TransmissionLoss TransmissionLoss { get; set; }
        [NotMapped]
        public float[] Ranges
        {
            get { return _ranges ?? (_ranges = RangeAxisBlob.ToArray()); }
            set
            {
                _ranges = value;
                RangeAxisBlob = _ranges.ToBlob();
            }
        }
        float[] _ranges;

        [NotMapped]
        public float[] Depths
        {
            get { return _depths ?? (_depths = DepthAxisBlob.ToArray()); }
            set
            {
                _depths = value;
                DepthAxisBlob = _depths.ToBlob();
            }
        }
        float[] _depths;

        [NotMapped]
        public BottomProfilePoint[] BottomProfile
        {
            get { return _bottomProfile ?? (_bottomProfile = BottomProfileBlob.ToBottomProfileArray()); }
            set
            {
                _bottomProfile = value;
                BottomProfileBlob = _bottomProfile.ToBlob();
            }
        }
        BottomProfilePoint[] _bottomProfile;

        [NotMapped]
        public GeoSegment Segment
        {
            get { return _segment ?? (_segment = new GeoSegment(TransmissionLoss.AnalysisPoint.Geo, Geo.KilometersToRadians(Length / 1000f), Geo.DegreesToRadians(Bearing))); }
        }
        GeoSegment _segment;

        [NotMapped]
        public float[] MinimumTransmissionLossValues
        {
            get { return _minimumTransmissionLossValues ?? (_minimumTransmissionLossValues = MinimumTransmissionLossBlob.ToArray()); }
            set
            {
                _minimumTransmissionLossValues = value;
                MinimumTransmissionLossBlob = _minimumTransmissionLossValues.ToBlob();
            }
        }
        float[] _minimumTransmissionLossValues;

        [NotMapped]
        public float[] MaximumTransmissionLossValues
        {
            get { return _maximumTransmissionLossValues ?? (_maximumTransmissionLossValues = MaximumTransmissionLossBlob.ToArray()); }
            set
            {
                _maximumTransmissionLossValues = value;
                MaximumTransmissionLossBlob = _maximumTransmissionLossValues.ToBlob();
            }
        }
        float[] _maximumTransmissionLossValues;

        [NotMapped]
        public float[] MeanTransmissionLossValues
        {
            get { return _meanTransmissionLossValues ?? (_meanTransmissionLossValues = MeanTransmissionLossBlob.ToArray()); }
            set
            {
                _meanTransmissionLossValues = value;
                MeanTransmissionLossBlob = _meanTransmissionLossValues.ToBlob();
            }
        }
        float[] _meanTransmissionLossValues;
    }
}

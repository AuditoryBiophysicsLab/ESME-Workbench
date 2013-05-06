using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Windows.Data;
using ESME.Locations;
using ESME.Mapping;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Scenarios
{
    [NotifyPropertyChanged]
    public class TransmissionLoss : IHaveGuid, IHaveLayerSettings
    {
        [Key, Initialize] public Guid Guid { get; set; }
        public TransmissionLoss() { Initialize(); }

        void Initialize()
        {
            Radials = new ObservableList<Radial>();
            var lineColor = LayerSettings.LineOrSymbolColor;
            lineColor.ScA = 0.5f;   // Set the default alpha channel for this TransmissionLoss to 50%
            LayerSettings.LineOrSymbolColor = lineColor;
            LayerSettings.DisplayIfScenarioIsLoadedFunc = () => AnalysisPoint.Scenario.IsLoaded;
        }

        [NotMapped] public bool HasErrors { get; private set; }
        [NotMapped] public string Errors { get; private set; }
        public void CheckForErrors()
        {
            var radialsWithErrors = (from radial in Radials
                                     orderby radial.Bearing
                                     where radial.HasErrors
                                     select radial).ToList();
            var sb = new StringBuilder();
            if (radialsWithErrors.Count > 0)
            {
                var maxMode = (from mode in Modes
                               orderby mode.MaxPropagationRadius descending
                               select mode).First();
                sb.AppendLine(string.Format("Sound field for mode {0}:{1}:{2}{3} contains errors", maxMode.Source.Platform.PlatformName, maxMode.Source.SourceName, maxMode.ModeName, Modes.Count == 1 ? "" : string.Format(" [and {0} other(s)]", Modes.Count - 1)));
                foreach (var radial in Modes.SelectMany(mode => radialsWithErrors)) sb.AppendLine(String.Format("  � {0}", radial.Errors));
            }
            Errors = sb.ToString().TrimEnd();
            if (!string.IsNullOrEmpty(Errors)) Debug.WriteLine(Errors);
            HasErrors = !string.IsNullOrEmpty(Errors);
            AnalysisPoint.CheckForErrors();
        }

        public bool IsReadyToCalculate { get; set; }
        public virtual AnalysisPoint AnalysisPoint { get; set; }
        [Initialize] public virtual ObservableList<Mode> Modes { get; set; }
        [Initialize] public virtual LayerSettings LayerSettings { get; set; }
        public virtual ObservableList<Radial> Radials { get; set; }
        [NotMapped]
        public string LayerName { get { return string.Format("[{0:0.###}, {1:0.###}]", AnalysisPoint.Geo.Latitude, AnalysisPoint.Geo.Longitude); } }
        [NotMapped] public bool IsDeleted { get; set; }

        #region ViewTransmissionLossCommand
        public SimpleCommand<object, EventToCommandArgs> ViewTransmissionLossCommand { get { return _viewTransmissionLoss ?? (_viewTransmissionLoss = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.ViewTransmissionLoss, this))); } }

        SimpleCommand<object, EventToCommandArgs> _viewTransmissionLoss;
        #endregion

        #region DeleteTransmissionLossCommand
        public SimpleCommand<object, EventToCommandArgs> DeleteTransmissionLossCommand { get { return _deleteTransmissionLoss ?? (_deleteTransmissionLoss = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeleteTransmissionLoss, this))); } }
        SimpleCommand<object, EventToCommandArgs> _deleteTransmissionLoss;
        #endregion

        #region RecalculateTransmissionLossCommand
        public SimpleCommand<object, EventToCommandArgs> RecalculateTransmissionLossCommand
        {
            get
            {
                return _recalculateTransmissionLoss ??
                       (_recalculateTransmissionLoss = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.RecalculateTransmissionLoss, this)));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _recalculateTransmissionLoss;
        #endregion

        /// <summary>
        /// Return the Radial closest to the specified bearing
        /// </summary>
        /// <param name="desiredBearing">Desired bearing, in degrees clockwise from true north</param>
        /// <returns></returns>
        public Radial ClosestRadial(double desiredBearing)
        {
            var minBearing = double.MaxValue;
            Radial closestRadial = null;
            foreach (var radial in Radials)
            {
                var bearing = radial.Bearing;
                while (bearing < 0) bearing += 360;
                bearing %= 360;
                var relBearing = Math.Abs(bearing - desiredBearing);
                if (relBearing >= minBearing) continue;
                minBearing = relBearing;
                closestRadial = radial;
            }
            return closestRadial;
        }

        volatile object _createMapLayerLock = new object();
        public void CreateMapLayers()
        {
            if (Modes == null || Modes.Count == 0)
            {
                Debug.WriteLine(string.Format("Deleting map layers for TL {0} at {1} because Modes is null or empty", Guid, (Geo)AnalysisPoint.Geo));
                Delete();
                return;
            }
            if (IsDeleted) return;
            lock (_createMapLayerLock)
            {
                if (IsDeleted) return;
                LayerSettings.IsChecked = false;
                var mapLayer = (LayerSettings.MapLayerViewModel != null) ? (OverlayShapeMapLayer)LayerSettings.MapLayerViewModel : new OverlayShapeMapLayer { Name = string.Format("{0}", Guid) };
                mapLayer.Clear();
                var maxPropagationRadius = Modes.Max(m => m.MaxPropagationRadius);
                Debug.WriteLine(string.Format("Creating map layers for TL {0} of radius {1} for mode [{2}] at {3}", Guid, maxPropagationRadius, Modes.First().ModeName, (Geo)AnalysisPoint.Geo));
                var perimeterGeos = new List<Geo>();
                var perimeterSegmentCount = Math.Max(Radials.Count, 32);
                for (var perimeterSegmentIndex = 0; perimeterSegmentIndex < perimeterSegmentCount; perimeterSegmentIndex++) perimeterGeos.Add(((Geo)AnalysisPoint.Geo).Offset(Geo.KilometersToRadians(maxPropagationRadius / 1000), Geo.DegreesToRadians((360.0 / perimeterSegmentCount) * perimeterSegmentIndex)));
                perimeterGeos.Add(perimeterGeos.First());
                var radialGeos = new List<Geo>();
                foreach (var radial in Radials.OrderBy(r => r.Bearing))
                {
                    radialGeos.Add(AnalysisPoint.Geo);
                    var radialEndGeo = ((Geo)AnalysisPoint.Geo).Offset(Geo.KilometersToRadians(maxPropagationRadius / 1000), Geo.DegreesToRadians(radial.Bearing));
                    radialGeos.Add(radialEndGeo);
                }
                radialGeos.Add(AnalysisPoint.Geo);
                mapLayer.AddLines(radialGeos);
                mapLayer.AddPolygon(perimeterGeos);
                mapLayer.Done();
                LayerSettings.MapLayerViewModel = mapLayer;
                if (AnalysisPoint.LayerSettings.IsChecked) LayerSettings.IsChecked = true;
            }
        }

        public void RemoveMapLayers()
        {
            if (Modes != null && Modes.Count > 0)
            {
                var maxPropagationRadius = Modes.Max(m => m.MaxPropagationRadius);
                Debug.WriteLine(string.Format("Removing map layers for TL {0} of radius {1} for mode [{2}] at {3}", Guid, maxPropagationRadius, Modes.First().ModeName, (Geo)AnalysisPoint.Geo));
            }
            LayerSettings.MapLayerViewModel = null;
        }

        public void Delete()
        {
            if (IsDeleted) return;
            lock (_createMapLayerLock)
            {
                if (IsDeleted) return;
                IsDeleted = true;
                RemoveMapLayers();
            }
            foreach (var radial in Radials.ToList()) radial.Delete();
            AnalysisPoint.TransmissionLosses.Remove(this);
            foreach (var mode in Modes) mode.TransmissionLosses.Remove(this); // Remove this TL from all matching modes' list of TLs
            if (AnalysisPoint.TransmissionLosses.Count == 0) AnalysisPoint.Delete();
            Scenario.Database.Context.LayerSettings.Remove(LayerSettings);
            Scenario.Database.Context.TransmissionLosses.Remove(this);
        }
        #region Layer Move commands
        #region MoveLayerToFrontCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerToFrontCommand { get { return _moveLayerToFront ?? (_moveLayerToFront = new SimpleCommand<object, EventToCommandArgs>(o => { LayerSettings.MoveLayerToFront(); MediatorMessage.Send(MediatorMessage.RefreshMap, true); })); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerToFront;
        #endregion

        #region MoveLayerForwardCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerForwardCommand { get { return _moveLayerForward ?? (_moveLayerForward = new SimpleCommand<object, EventToCommandArgs>(o => { LayerSettings.MoveLayerForward(); MediatorMessage.Send(MediatorMessage.RefreshMap, true); })); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerForward;
        #endregion

        #region MoveLayerBackwardCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerBackwardCommand { get { return _moveLayerBackward ?? (_moveLayerBackward = new SimpleCommand<object, EventToCommandArgs>(o => { LayerSettings.MoveLayerBackward(); MediatorMessage.Send(MediatorMessage.RefreshMap, true); })); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerBackward;
        #endregion

        #region MoveLayerToBackCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerToBackCommand { get { return _moveLayerToBack ?? (_moveLayerToBack = new SimpleCommand<object, EventToCommandArgs>(o => { LayerSettings.MoveLayerToBack(); MediatorMessage.Send(MediatorMessage.RefreshMap, true); })); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerToBack;
        #endregion
        #endregion

        public void Recalculate()
        {
            Debug.WriteLine(string.Format("Recalculating transmission loss at ({0:0.###}, {1:0.###}) for mode {2}", AnalysisPoint.Geo.Latitude, AnalysisPoint.Geo.Longitude, Modes[0]));
            var maxPropagationRadius = Modes.Max(m => m.MaxPropagationRadius);
            RemoveMapLayers();
            foreach (var radial in Radials)
            {
                radial.Length = maxPropagationRadius;
                radial.Recalculate();
                Debug.WriteLine(string.Format("Recalculating radial at bearing {0} with radius of {1}", radial.Bearing, radial.Length));
            }
            CreateMapLayers();
        }

        public override string ToString()
        {
            var sb = new StringBuilder();
            if (Modes == null || Modes.Count == 0) sb.AppendLine("(no modes)");
            else sb.AppendLine("Mode: " + Modes.First());
            if (Radials == null || Radials.Count == 0) sb.AppendLine("(no radials)");
            else foreach (var radial in Radials.OrderBy(r => r.Bearing)) sb.AppendLine(radial.ToString());
            return sb.ToString();
        }
    }

    [ValueConversion(typeof(TransmissionLoss), typeof(string))]
    public class TransmissionLossGroupingConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture) { return value.ToString(); }
        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture) { throw new NotImplementedException(); }
    }

}
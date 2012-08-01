using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Input;
using System.Windows.Threading;
using ESME.Database;
using ESME.Locations;
using ESME.Mapping;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Scenarios
{
    [NotifyPropertyChanged]
    public class AnalysisPoint : IHaveGuid, IHaveLayerSettings, INotifyPropertyChanged
    {
        [Key, Initialize] public Guid Guid { get; set; }
        public DbGeo Geo { get; set; }

        public virtual Scenario Scenario { get; set; }
        [Initialize] public virtual LayerSettings LayerSettings { get; set; }
        [Initialize] public virtual ObservableList<TransmissionLoss> TransmissionLosses { get; set; }

        [NotMapped] public string LayerName { get { return string.Format("[{0:0.###}, {1:0.###}]", Geo.Latitude, Geo.Longitude); } }
        #region INotifyPropertyChanged implementation
        public event PropertyChangedEventHandler PropertyChanged;
        protected void OnPropertyChanged(string propertyName)
        {
            var handlers = PropertyChanged;
            if (handlers == null) return;
            foreach (PropertyChangedEventHandler handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                {
                    var localHandler = handler;
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => localHandler(this, new PropertyChangedEventArgs(propertyName)));
                }
                else
                    handler(this, new PropertyChangedEventArgs(propertyName));
            }
        }
        #endregion

        #region ViewAnalysisPointCommand
        public SimpleCommand<object, EventToCommandArgs> ViewAnalysisPointCommand
        {
            get
            {
                return _viewAnalysisPoint ?? (_viewAnalysisPoint = new SimpleCommand<object, EventToCommandArgs>(o =>
                {
                    MediatorMessage.Send(MediatorMessage.ViewAnalysisPoint, this);
                    ((MouseEventArgs)o.EventArgs).Handled = true;
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _viewAnalysisPoint;
        #endregion

        #region DeleteAnalysisPointCommand
        public SimpleCommand<object, EventToCommandArgs> DeleteAnalysisPointCommand { get { return _deleteAnalysisPoint ?? (_deleteAnalysisPoint = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeleteAnalysisPoint, this))); } }
        SimpleCommand<object, EventToCommandArgs> _deleteAnalysisPoint;
        #endregion

        #region RecalculateAnalysisPointCommand
        public SimpleCommand<object, EventToCommandArgs> RecalculateAnalysisPointCommand
        {
            get
            {
                return _recalculateAnalysisPoint ??
                       (_recalculateAnalysisPoint = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.RecalculateAnalysisPoint, this)));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _recalculateAnalysisPoint;
        #endregion

        #region AnalysisPointPropertiesCommand
        public SimpleCommand<object, EventToCommandArgs> AnalysisPointPropertiesCommand
        {
            get
            {
                return _analysisPointProperties ??
                       (_analysisPointProperties = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.ViewAnalysisPointProperties, this)));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _analysisPointProperties;
        #endregion

        [NotMapped] public bool IsDeleted { get; set; }

        public void CreateMapLayers()
        {
            if (IsDeleted) return;
            if (Scenario.ShowAllAnalysisPoints) LayerSettings.IsChecked = true;
            LayerSettings.PropertyChanged += LayerSettingsChanged;
            foreach (var transmissionLoss in TransmissionLosses.ToList()) transmissionLoss.CreateMapLayers();
        }

        public void RemoveMapLayers()
        {
            foreach (var transmissionLoss in TransmissionLosses.ToList()) transmissionLoss.RemoveMapLayers();
            LayerSettings.PropertyChanged -= LayerSettingsChanged;
        }

        void LayerSettingsChanged(object sender, PropertyChangedEventArgs args)
        {
            if (args.PropertyName != "IsChecked") return;
            var checkState = LayerSettings.IsChecked;
            foreach (var transmissionLoss in TransmissionLosses) transmissionLoss.LayerSettings.IsChecked = checkState;
        }

        public void Delete()
        {
            if (IsDeleted) return;
            IsDeleted = true;
            LayerSettings.IsChecked = false;
            foreach (var transmissionLoss in TransmissionLosses) transmissionLoss.LayerSettings.IsChecked = false;
            RemoveMapLayers();
            foreach (var tl in TransmissionLosses.ToList()) tl.Delete();
            if (Scenario.AnalysisPoints.Contains(this)) Scenario.AnalysisPoints.Remove(this);
            Scenario.Database.Context.LayerSettings.Remove(LayerSettings);
            Scenario.Database.Context.AnalysisPoints.Remove(this);
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
    }

    [NotifyPropertyChanged]
    public class TransmissionLoss : IHaveGuid, IHaveLayerSettings, INotifyPropertyChanged
    {
        [Key, Initialize] public Guid Guid { get; set; }

        public TransmissionLoss() { Initialize(); }

        void Initialize()
        {
            var lineColor = LayerSettings.LineOrSymbolColor;
            lineColor.ScA = 0.5f;   // Set the default alpha channel for this TransmissionLoss to 50%
            LayerSettings.LineOrSymbolColor = lineColor;
        }

        public bool IsReadyToCalculate { get; set; }
        public virtual AnalysisPoint AnalysisPoint { get; set; }
        public virtual Mode Mode { get; set; }
        [Initialize] public virtual LayerSettings LayerSettings { get; set; }
        [Initialize] public virtual ObservableList<Radial> Radials { get; set; }
        #region INotifyPropertyChanged implementation
        public event PropertyChangedEventHandler PropertyChanged;
        protected void OnPropertyChanged(string propertyName)
        {
            var handlers = PropertyChanged;
            if (handlers == null) return;
            foreach (PropertyChangedEventHandler handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                {
                    var localHandler = handler;
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => localHandler(this, new PropertyChangedEventArgs(propertyName)));
                }
                else
                    handler(this, new PropertyChangedEventArgs(propertyName));
            }
        }
        #endregion
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
            if (Mode == null)
            {
                Delete();
                return;
            }
            if (IsDeleted) return;
            lock (_createMapLayerLock)
            {
                if (IsDeleted) return;
                LayerSettings.IsChecked = false;
                var mapLayer = new OverlayShapeMapLayer { Name = string.Format("{0}", Guid) };
                mapLayer.Clear();
                var geos = new List<Geo>();
                foreach (var radial in Radials)
                {
                    geos.Add(AnalysisPoint.Geo);
                    geos.Add(((Geo)AnalysisPoint.Geo).Offset(Geo.KilometersToRadians(Mode.MaxPropagationRadius / 1000), Geo.DegreesToRadians(radial.Bearing)));
                }
                geos.Add(AnalysisPoint.Geo);
                mapLayer.AddLines(geos);
                mapLayer.Done();
                LayerSettings.MapLayerViewModel = mapLayer;
                if (AnalysisPoint.LayerSettings.IsChecked) LayerSettings.IsChecked = true;
            }
        }

        public void RemoveMapLayers() { LayerSettings.MapLayerViewModel = null; }

        public void Delete()
        {
            if (IsDeleted) return;
            lock (_createMapLayerLock)
            {
                IsDeleted = true;
                RemoveMapLayers();
            }
            foreach (var radial in Radials.ToList()) radial.Delete();
            AnalysisPoint.TransmissionLosses.Remove(this);
            if (Mode != null) Mode.TransmissionLosses.Remove(this);
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
    }

    [NotifyPropertyChanged]
    public class Radial : IHaveGuid
    {
        public Radial() { Filename = Path.GetFileNameWithoutExtension(Path.GetRandomFileName()); }
        [Key, Initialize] public Guid Guid { get; set; }
        public bool IsCalculated { get; set; }
        public string Filename { get; set; }
        public DbDateTime CalculationStarted { get; set; }
        public DbDateTime CalculationCompleted { get; set; }

        /// <summary>
        ///   In degrees, clockwise from true north
        /// </summary>
        public double Bearing { get; set; }

        /// <summary>
        ///   In meters
        /// </summary>
        public double Length { get; set; }

        public virtual TransmissionLoss TransmissionLoss { get; set; }

        /// <summary>
        /// Returns the size on disk of all files used by this radial
        /// </summary>
        [NotMapped]
        public long FileSize
        {
            get
            {
                if (string.IsNullOrEmpty(Filename) || TransmissionLoss == null || TransmissionLoss.AnalysisPoint == null || TransmissionLoss.AnalysisPoint.Scenario == null || string.IsNullOrEmpty(TransmissionLoss.AnalysisPoint.Scenario.StorageDirectoryPath)) return 0;
                if (!Directory.Exists(TransmissionLoss.AnalysisPoint.Scenario.StorageDirectoryPath)) return 0;
                var files = Directory.GetFiles(TransmissionLoss.AnalysisPoint.Scenario.StorageDirectoryPath, Filename + ".*");
                return files.Select(file => new FileInfo(file)).Select(fi => fi.Length).Sum();
            }
        }

        [NotMapped] public string BasePath
        {
            get
            {
                if (Filename == null || Filename.EndsWith(".shd")) Filename = Path.GetFileNameWithoutExtension(Path.GetRandomFileName());
                return TransmissionLoss == null || TransmissionLoss.AnalysisPoint == null ? null : Path.Combine(TransmissionLoss.AnalysisPoint.Scenario.StorageDirectoryPath, Filename);
            }
        }

        [NotMapped] public float[] Ranges
        {
            get
            {
                if (_ranges == null) ReadAxisFile();
                return _ranges;
            }
        }
        float[] _ranges;

        [NotMapped] public float[] Depths
        {
            get
            {
                if (_depths == null) ReadAxisFile();
                return _depths;
            }
        }
        float[] _depths;

        public void Delete()
        {
            if (IsDeleted) return;
            IsDeleted = true;
            var files = Directory.GetFiles(Path.GetDirectoryName(BasePath), Path.GetFileNameWithoutExtension(BasePath) + ".*");
            foreach (var file in files) File.Delete(file);
            TransmissionLoss.Radials.Remove(this);
            Scenario.Database.Context.Radials.Remove(this);
        }

        [NotMapped, Initialize] public ObservableList<string> Errors { get; set; }
        [NotMapped] public BottomProfilePoint[] BottomProfile { get { return _bottomProfile ?? (_bottomProfile = ESME.TransmissionLoss.Bellhop.BottomProfile.FromBellhopFile(BasePath + ".bty")); } }
        BottomProfilePoint[] _bottomProfile;

        [NotMapped] public GeoSegment Segment { get { return _segment ?? (_segment = new GeoSegment(TransmissionLoss.AnalysisPoint.Geo, Length, Bearing)); } }
        GeoSegment _segment;

        [NotMapped] public bool IsDeleted { get; set; }

        [NotMapped] public float[] MinimumTransmissionLossValues
        {
            get
            {
                if (_minimumTransmissionLossValues == null) ReadAxisFile();
                return _minimumTransmissionLossValues;
            }
        }

        float[] _minimumTransmissionLossValues;

        [NotMapped] public float[] MaximumTransmissionLossValues
        {
            get
            {
                if (_maximumTransmissionLossValues == null) ReadAxisFile();
                return _maximumTransmissionLossValues;
            }
        }

        float[] _maximumTransmissionLossValues;

        [NotMapped] public float[] MeanTransmissionLossValues
        {
            get
            {
                if (_meanTransmissionLossValues == null) ReadAxisFile();
                return _meanTransmissionLossValues;
            }
        }

        float[] _meanTransmissionLossValues;

        public async Task<Radial> LoadAsync()
        {
            var result = new Task(() =>
                                  {
                                      if (_bottomProfile == null) _bottomProfile = ESME.TransmissionLoss.Bellhop.BottomProfile.FromBellhopFile(BasePath + ".bty");
                                      if (_ranges == null || _depths == null) ReadAxisFile();
                                      if (_transmissionLossRadial == null) _transmissionLossRadial = new TransmissionLossRadial((float)Bearing, new BellhopOutput(BasePath + ".shd"));
                                  });
            result.Start();
            await TaskEx.WhenAll(result);
            return this;
        }

        TransmissionLossRadial _transmissionLossRadial;

        [NotMapped] public TransmissionLossRadial TransmissionLossRadial
        {
            get
            {
                if (_transmissionLossRadial != null) return _transmissionLossRadial;
                _transmissionLossRadial = new TransmissionLossRadial((float)Bearing, new BellhopOutput(BasePath + ".shd"));
                return _transmissionLossRadial;
            }
        }

        public void ExtractAxisData(TransmissionLossRadial transmissionLoss = null)
        {
            if (transmissionLoss == null) transmissionLoss = new TransmissionLossRadial((float)Bearing, new BellhopOutput(BasePath + ".shd"));
            _ranges = transmissionLoss.Ranges.ToArray();
            _depths = transmissionLoss.Depths.ToArray();
            IsCalculated = true;
            _bottomProfile = ESME.TransmissionLoss.Bellhop.BottomProfile.FromBellhopFile(BasePath + ".bty");
            _minimumTransmissionLossValues = new float[Ranges.Length];
            _maximumTransmissionLossValues = new float[Ranges.Length];
            _meanTransmissionLossValues = new float[Ranges.Length];
            for (var rangeIndex = 0; rangeIndex < Ranges.Length; rangeIndex++)
            {
                MinimumTransmissionLossValues[rangeIndex] = transmissionLoss[rangeIndex].Min();
                MaximumTransmissionLossValues[rangeIndex] = transmissionLoss[rangeIndex].Max();
                MeanTransmissionLossValues[rangeIndex] = transmissionLoss[rangeIndex].Average();
            }
            using (var writer = new BinaryWriter(new FileStream(BasePath + ".axs", FileMode.Create)))
            {
                writer.Write(_ranges.Length);
                foreach (var range in _ranges) writer.Write(range);
                writer.Write(_depths.Length);
                foreach (var depth in _depths) writer.Write(depth);
                writer.Write(_minimumTransmissionLossValues.Length);
                foreach (var tl in _minimumTransmissionLossValues) writer.Write(tl);
                writer.Write(_maximumTransmissionLossValues.Length);
                foreach (var tl in _maximumTransmissionLossValues) writer.Write(tl);
                writer.Write(_meanTransmissionLossValues.Length);
                foreach (var tl in _meanTransmissionLossValues) writer.Write(tl);
            }
            //MediatorMessage.Send(MediatorMessage.TransmissionLossLayerChanged, TransmissionLoss);
        }

        void ReadAxisFile()
        {
            if (!File.Exists(BasePath + ".axs")) ExtractAxisData();
            using (var reader = new BinaryReader(new FileStream(BasePath + ".axs", FileMode.Open)))
            {
                _ranges = new float[reader.ReadInt32()];
                for (var i = 0; i < _ranges.Length; i++) _ranges[i] = reader.ReadSingle();
                _depths = new float[reader.ReadInt32()];
                for (var i = 0; i < _depths.Length; i++) _depths[i] = reader.ReadSingle();

                _minimumTransmissionLossValues = new float[reader.ReadInt32()];
                for (var i = 0; i < _minimumTransmissionLossValues.Length; i++) _minimumTransmissionLossValues[i] = reader.ReadSingle();

                _maximumTransmissionLossValues = new float[reader.ReadInt32()];
                for (var i = 0; i < _maximumTransmissionLossValues.Length; i++) _maximumTransmissionLossValues[i] = reader.ReadSingle();

                _meanTransmissionLossValues = new float[reader.ReadInt32()];
                for (var i = 0; i < _meanTransmissionLossValues.Length; i++) _meanTransmissionLossValues[i] = reader.ReadSingle();
            }
        }
    }
}
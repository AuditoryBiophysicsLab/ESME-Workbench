using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows.Input;
using ESME.Database;
using ESME.Environment;
using ESME.Locations;
using ESME.Mapping;
using ESME.Model;
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
    public class AnalysisPoint : IHaveGuid, IHaveLayerSettings, ISupportValidation, INotifyPropertyChanged
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public DbGeo Geo { get; set; }

        public virtual Scenario Scenario { get; set; }
        public virtual LayerSettings LayerSettings { get; set; }

        [Initialize]
        public virtual ObservableList<TransmissionLoss> TransmissionLosses { get; set; }

        public void CreateMapLayers()
        {
            if (LayerSettings == null) LayerSettings = new LayerSettings();
            LayerSettings.PropertyChanged += LayerSettingsChanged;
            foreach (var transmissionLoss in TransmissionLosses) transmissionLoss.CreateMapLayers();
        }

        public void RemoveMapLayers() { LayerSettings.PropertyChanged -= LayerSettingsChanged; }

        void LayerSettingsChanged(object sender, PropertyChangedEventArgs args)
        {
            if (args.PropertyName != "IsChecked") return;
            var checkState = LayerSettings.IsChecked;
            foreach (var transmissionLoss in TransmissionLosses) transmissionLoss.LayerSettings.IsChecked = checkState;
        }
        public bool IsValid
        {
            get { throw new NotImplementedException(); }
        }

        public string ValidationErrorText
        {
            get { throw new NotImplementedException(); }
        }

        [NotMapped] public string LayerName { get { return string.Format("[{0:0.###}, {1:0.###}]", Geo.Latitude, Geo.Longitude); } }
        public void Validate() { throw new NotImplementedException(); }
        public event PropertyChangedEventHandler PropertyChanged;
        #region ViewAnalysisPointCommand
        public SimpleCommand<object, EventToCommandArgs> ViewAnalysisPointCommand
        {
            get { return _viewAnalysisPoint ?? (_viewAnalysisPoint = new SimpleCommand<object, EventToCommandArgs>(o =>
            {
                MediatorMessage.Send(MediatorMessage.ViewAnalysisPoint, this);
                ((MouseEventArgs)o.EventArgs).Handled = true;
            })); }
        }

        SimpleCommand<object, EventToCommandArgs> _viewAnalysisPoint;
        #endregion

        #region DeleteAnalysisPointCommand
        public SimpleCommand<object, EventToCommandArgs> DeleteAnalysisPointCommand { get { return _deleteAnalysisPoint ?? (_deleteAnalysisPoint = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeleteAnalysisPoint, this))); } }
        SimpleCommand<object, EventToCommandArgs> _deleteAnalysisPoint;
        #endregion

        #region RecalculateAnalysisPointCommand
        public SimpleCommand<object, EventToCommandArgs> RecalculateAnalysisPointCommand { get { return _recalculateAnalysisPoint ?? (_recalculateAnalysisPoint = new SimpleCommand<object, EventToCommandArgs>(o=>MediatorMessage.Send(MediatorMessage.RecalculateAnalysisPoint,this))); } }
        SimpleCommand<object, EventToCommandArgs> _recalculateAnalysisPoint;
        #endregion
        public void Delete()
        {
            RemoveMapLayers();
            foreach (var tl in TransmissionLosses.ToList()) tl.Delete();
            if (Scenario.AnalysisPoints.Contains(this)) Scenario.AnalysisPoints.Remove(this);
        }
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

        [Initialize]
        public virtual ObservableList<Radial> Radials { get; set; }

        [NotMapped]
        public string LayerName { get { return string.Format("[{0:0.###}, {1:0.###}]", AnalysisPoint.Geo.Latitude, AnalysisPoint.Geo.Longitude); } }

        public void Validate()
        {
            if (Scenario.Database == null || Scenario.Cache == null || AnalysisPoint == null || AnalysisPoint.Scenario.Bathymetry == null)
            {
                ValidationErrorText = "Unable to validate";
                return;
            }
            var geoRect = ((Bathymetry)Scenario.Cache[AnalysisPoint.Scenario.Bathymetry].Result).Samples.GeoRect;

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
        #region ViewTransmissionLossCommand
        public SimpleCommand<object, EventToCommandArgs> ViewTransmissionLossCommand
        {
            get { return _viewTransmissionLoss ?? (_viewTransmissionLoss = new SimpleCommand<object, EventToCommandArgs>(o =>  MediatorMessage.Send(MediatorMessage.ViewTransmissionLoss, this))); }
        }

        SimpleCommand<object, EventToCommandArgs> _viewTransmissionLoss;
        #endregion

        #region DeleteTransmissionLossCommand
        public SimpleCommand<object, EventToCommandArgs> DeleteTransmissionLossCommand { get { return _deleteTransmissionLoss ?? (_deleteTransmissionLoss = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeleteTransmissionLoss, this))); } }
        SimpleCommand<object, EventToCommandArgs> _deleteTransmissionLoss;
        #endregion

        #region RecalculateTransmissionLossCommand
        public SimpleCommand<object, EventToCommandArgs> RecalculateTransmissionLossCommand { get { return _recalculateTransmissionLoss ?? (_recalculateTransmissionLoss = new SimpleCommand<object, EventToCommandArgs>(o=>MediatorMessage.Send(MediatorMessage.RecalculateTransmissionLoss,this))); } }
        SimpleCommand<object, EventToCommandArgs> _recalculateTransmissionLoss;
        #endregion

        [Affects("IsValid")]
        [NotMapped]
        public string ValidationErrorText
        {
            get { Validate(); return _validationErrorText; }
            private set { _validationErrorText = value; }
        }
        string _validationErrorText;

        public void CreateMapLayers()
        {
            var mapLayer = new OverlayShapeMapLayer
            {
                LayerType = LayerType.Track,
                Name = string.Format("{0}", Guid),
            };
            var geos = new List<Geo>();
            foreach (var radial in Radials)
            {
                geos.Add(radial.Segment[0]);
                geos.Add(radial.Segment[1]);
            }
            geos.Add(geos[0]);
            mapLayer.AddLines(geos);
            mapLayer.Done();
            if (LayerSettings == null) LayerSettings = new LayerSettings();
            LayerSettings.MapLayerViewModel = mapLayer;
        }
        public void RemoveMapLayers() { LayerSettings.MapLayerViewModel = null; }

        public void Delete()
        {
            RemoveMapLayers();
            foreach (var radial in Radials.ToList()) radial.Delete();
            Mode.TransmissionLosses.Remove(this);
            AnalysisPoint.TransmissionLosses.Remove(this);
            if (AnalysisPoint.TransmissionLosses.Count == 0) AnalysisPoint.Scenario.AnalysisPoints.Remove(AnalysisPoint);
        }
    }

    [NotifyPropertyChanged]
    public class Radial : IHaveGuid
    {
        public Radial() { Filename = Path.GetFileNameWithoutExtension(Path.GetRandomFileName()); }
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

        public virtual TransmissionLoss TransmissionLoss { get; set; }

        [NotMapped]
        public string BasePath
        {
            get
            {
                if (Filename == null || Filename.EndsWith(".shd")) Filename = Path.GetFileNameWithoutExtension(Path.GetRandomFileName());
                return TransmissionLoss == null ? null : Path.Combine(TransmissionLoss.AnalysisPoint.Scenario.StorageDirectoryPath, Filename);
            }
        }

        [NotMapped]
        public float[] Ranges
        {
            get
            {
                if (_ranges == null) ReadAxisFile();
                return _ranges;
            }
        }
        float[] _ranges;

        [NotMapped]
        public float[] Depths
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
            var files = Directory.GetFiles(Path.GetDirectoryName(BasePath), Path.GetFileNameWithoutExtension(BasePath) + ".*");
            foreach (var file in files) File.Delete(file);
            TransmissionLoss.Radials.Remove(this);
        }

        [NotMapped]
        public BottomProfilePoint[] BottomProfile
        {
            get { return _bottomProfile ?? (_bottomProfile = ESME.TransmissionLoss.Bellhop.BottomProfile.FromBellhopFile(BasePath + ".bty")); }
        }
        BottomProfilePoint[] _bottomProfile;

        [NotMapped]
        public GeoSegment Segment
        {
            get { return _segment ?? (_segment = new GeoSegment(TransmissionLoss.AnalysisPoint.Geo, Length, Bearing)); }
        }
        GeoSegment _segment;

        [NotMapped]
        public float[] MinimumTransmissionLossValues
        {
            get
            {
                if (_minimumTransmissionLossValues == null) ReadAxisFile();
                return _minimumTransmissionLossValues;
            }
        }
        float[] _minimumTransmissionLossValues;

        [NotMapped]
        public float[] MaximumTransmissionLossValues
        {
            get
            {
                if (_maximumTransmissionLossValues == null) ReadAxisFile();
                return _maximumTransmissionLossValues;
            }
        }
        float[] _maximumTransmissionLossValues;

        [NotMapped]
        public float[] MeanTransmissionLossValues
        {
            get
            {
                if (_meanTransmissionLossValues == null) ReadAxisFile();
                return _meanTransmissionLossValues;
            }
        }
        float[] _meanTransmissionLossValues;

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
            MediatorMessage.Send(MediatorMessage.TransmissionLossLayerChanged, TransmissionLoss);
        }

        void ReadAxisFile()
        {
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

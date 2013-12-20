using System;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Windows.Input;
using ESME.Database;
using ESME.Locations;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Scenarios
{
    [NotifyPropertyChanged]
    public class AnalysisPoint : IHaveGuid, IHaveLayerSettings
    {
        public AnalysisPoint() { Initialize(); }
        public AnalysisPoint(AnalysisPoint analysisPoint): this() { Copy(analysisPoint); }
        void Copy(AnalysisPoint analysisPoint)
        {
            Geo = new Geo(analysisPoint.Geo);
            LayerSettings = new LayerSettings(analysisPoint.LayerSettings);
        }
        void Initialize()
        {
            TransmissionLosses = new ObservableList<TransmissionLoss>();
        }

        [Key, Initialize] public Guid Guid { get; set; }
        public DbGeo Geo { get; set; }

        public virtual Scenario Scenario { get; set; }
        [Initialize] public virtual LayerSettings LayerSettings { get; set; }
        public virtual ObservableList<TransmissionLoss> TransmissionLosses { get; set; }

        Geo _mouseHoverGeo;
        [NotMapped] public Geo MouseHoverGeo
        {
            get { return _mouseHoverGeo; }
            set
            {
                _mouseHoverGeo = value;
                TransmissionLosses.ForEach(t => t.MouseHoverGeo = _mouseHoverGeo);
            }
        }
        [NotMapped] public string LayerName { get { return string.Format("[{0:0.###}, {1:0.###}]", Geo.Latitude, Geo.Longitude); } }
        [NotMapped] public bool HasErrors { get; set; }
        [NotMapped] public string Errors { get; set; }
        public void CheckForErrors()
        {
            var transmissionLossesWithErrors = (from transmissionLoss in TransmissionLosses
                                                where transmissionLoss.HasErrors && transmissionLoss.Modes != null && transmissionLoss.Modes.Count > 0
                                                orderby transmissionLoss.Modes[0].HighFrequency
                                                select transmissionLoss).ToList();
            var sb = new StringBuilder();
            if (transmissionLossesWithErrors.Count > 0)
            {
                sb.AppendLine(string.Format("Analysis point at {0} has {1}:", (Geo)Geo, transmissionLossesWithErrors.Count == 1 ? "an error" : "errors"));
                foreach (var transmissionLoss in transmissionLossesWithErrors) sb.AppendLine("+ " + transmissionLoss.Errors);
            }
            Errors = sb.ToString().TrimEnd();
            if (!string.IsNullOrEmpty(Errors)) Debug.WriteLine(Errors);
            HasErrors = !string.IsNullOrEmpty(Errors);
        }

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

        public void UpdateMapLayers()
        {
            if (IsDeleted) return;
            if (Scenario.ShowAllAnalysisPoints) LayerSettings.IsChecked = true;
            LayerSettings.PropertyChanged += LayerSettingsChanged;
            foreach (var transmissionLoss in TransmissionLosses.ToList()) transmissionLoss.UpdateMapLayers();
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

        public void Recalculate()
        {
            Debug.WriteLine(string.Format("Recalculating analysis point at ({0:0.###}, {1:0.###})", Geo.Latitude, Geo.Longitude));

            foreach (var tl in TransmissionLosses) tl.Recalculate();
        }
    }
}
using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Windows.Input;
using System.Windows.Media;
using ESME.Database;
using ESME.Locations;
using ESME.Mapping;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;
using ThinkGeo.MapSuite.Core;

namespace ESME.Scenarios
{
    public class Platform : IHaveGuid, IHaveLayerSettings
    {
        [Key, Initialize] public Guid Guid { get; set; }
        public string Description { get; set; }
        public bool Launches { get; set; }
        public bool Tows { get; set; }
        public int RepeatCount { get; set; }

        // Copied from the PSM Platform
        public string PSMPlatformGuid { get; set; }
        public string PlatformName { get; set; }
        public string PlatformType { get; set; }

        public DbTrackType TrackType { get; set; }
        public DbGeo Geo { get; set; }
        public bool IsRandom { get; set; }
        public float Depth { get; set; }
        public float Course { get; set; }

        /// <summary>
        ///   Speed in knots (nautical miles per hour)
        /// </summary>
        public float Speed { get; set; }

        public virtual Scenario Scenario { get; set; }
        public virtual Perimeter Perimeter { get; set; }
        public virtual LayerSettings LayerSettings { get; set; }
        [Initialize] public virtual ObservableList<Source> Sources { get; set; }
        [Initialize] public virtual ObservableList<LogEntry> Logs { get; set; }
        [NotMapped] public bool IsNew { get; set; }
        [NotMapped] public object LayerControl
        {
            get { return _layerControl; }
            set
            {
                _layerControl = value;
                MediatorMessage.Send(MediatorMessage.PlatformBoundToLayer, this);
            }
        }
        object _layerControl;


        #region PlatformPropertiesCommand
        public SimpleCommand<object, EventToCommandArgs> PlatformPropertiesCommand { get { return _platformProperties ?? (_platformProperties = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.PlatformProperties, this))); } }
        SimpleCommand<object, EventToCommandArgs> _platformProperties;
        #endregion

        #region DeletePlatformCommand
        public SimpleCommand<object, EventToCommandArgs> DeletePlatformCommand { get { return _deletePlatform ?? (_deletePlatform = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeletePlatform, this))); } }
        SimpleCommand<object, EventToCommandArgs> _deletePlatform;
        #endregion

        public void Delete()
        {
            RemoveMapLayers();
            foreach (var source in Sources.ToList()) source.Delete();
            Scenario.Platforms.Remove(this);
        }

        #region AddSourceCommand
        public SimpleCommand<object, EventToCommandArgs> AddSourceCommand { get { return _addSource ?? (_addSource = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.AddSource, this))); } }
        SimpleCommand<object, EventToCommandArgs> _addSource;
        #endregion

        #region KeyUpCommand
        public SimpleCommand<object, EventToCommandArgs> KeyUpCommand { get { return _keyUp ?? (_keyUp = new SimpleCommand<object, EventToCommandArgs>(KeyUpHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _keyUp;

        void KeyUpHandler(EventToCommandArgs args)
        {
            var parameter = (KeyEventArgs)args.EventArgs;
            switch(parameter.Key)
            {
                case Key.Delete:
                    MediatorMessage.Send(MediatorMessage.DeletePlatform, this);
                    break;
                case Key.Insert:
                    MediatorMessage.Send(MediatorMessage.AddSource,this);
                    break;
                default:
                    return;
            }
        }
        #endregion

        public void CreateMapLayers()
        {
            if (LayerSettings == null) LayerSettings = new LayerSettings();
            var mapLayer = new OverlayShapeMapLayer
            {
                LayerType = LayerType.Track,
                Name = string.Format("{0}", Guid),
                CustomLineStyle =
                    new CustomStartEndLineStyle(PointSymbolType.Circle, Colors.Green, 5, PointSymbolType.Square, Colors.Red, 5, LayerSettings.LineOrSymbolColor, (float)LayerSettings.LineOrSymbolSize)
            };

            mapLayer.AddLines(new List<Geo> { Geo, ((Geo)Geo).Offset(HRC.Navigation.Geo.KilometersToRadians(25), HRC.Navigation.Geo.DegreesToRadians(90)) });
            mapLayer.Done();
            LayerSettings.MapLayerViewModel = mapLayer;
            if (Perimeter != null) Perimeter.CreateMapLayers();
        }

        public void RemoveMapLayers() { LayerSettings.MapLayerViewModel = null; }
    }
}
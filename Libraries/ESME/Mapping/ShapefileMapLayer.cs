using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Xml.Serialization;
using Cinch;
using HRC.ViewModels;
using ThinkGeo.MapSuite.Core;

namespace ESME.Mapping
{
    [Serializable]
    public class ShapefileMapLayer : MapLayerViewModel
    {
        #region Menu Initializers

        readonly MenuItemViewModelBase _areaColorMenu = new MenuItemViewModelBase
        {
            Header = "Area Color",
        };

        #endregion

        public ShapefileMapLayer()
        {
            LayerType = LayerType.Shapefile; 
            LayerOverlay.Layers.Clear();
            _areaColorMenu.Command = new SimpleCommand<object, object>(obj => CanChangeAreaColor, obj =>
            {
                var result = ColorPickerService.ShowDialog();
                if (!result.HasValue || !result.Value) return;
                AreaColor = ColorPickerService.Color;
                MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                MediatorMessage.Send(MediatorMessage.RefreshLayer, this);
            });
            AreaColorPickerMenu = new List<MenuItemViewModelBase>
            {
                                      _areaColorMenu,
                                  };

            ColorMenu.Children.Add(_areaColorMenu);
        }

        #region public string ShapefileName { get; set; }
        [XmlElement]
        public string ShapefileName
        {
            get { return _shapefileName; }
            set
            {
                if (_shapefileName == value) return;
                _shapefileName = value;

                switch (LayerType)
                {
                    case LayerType.Shapefile:
                        Name = Path.GetFileNameWithoutExtension(_shapefileName);
                        break;
                    case LayerType.BaseMap:
                        Name = "Base Map";
                        break;
                }
                    
                string projection = null;
                var projectionFile = Path.Combine(Path.GetDirectoryName(_shapefileName), "projection.txt");
                if (File.Exists(projectionFile))
                {
                    using (var sr = new StreamReader(projectionFile)) projection = sr.ReadToEnd();
                }
                var newLayer = new ShapeFileFeatureLayer(_shapefileName);
#if true
                if (AreaStyle == null)
                {
                    newLayer.ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = AreaStyles.County1;
                    newLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
                }
                else
                {
                    newLayer.ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = AreaStyle;
                    newLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
                }
#endif
                newLayer.RequireIndex = false;
                if (projection != null)
                    newLayer.FeatureSource.Projection = new ManagedProj4Projection
                    {
                        InternalProjectionParametersString = projection,
                        ExternalProjectionParametersString = ManagedProj4Projection.GetEpsgParametersString(4326),
                    };
                LayerOverlay.Layers.Add(newLayer);
            }
        }

        string _shapefileName;

        #endregion

        #region public List<MenuItemViewModelBase> AreaColorPickerMenu { get; set; }
        [XmlIgnore]
        public List<MenuItemViewModelBase> AreaColorPickerMenu
        {
            get { return _areaColorPickerMenu; }
            set
            {
                if (_areaColorPickerMenu == value) return;
                _areaColorPickerMenu = value;
                OnPropertyChanged(AreaColorPickerMenuChangedEventArgs);
            }
        }
        static readonly PropertyChangedEventArgs AreaColorPickerMenuChangedEventArgs = ObservableHelper.CreateArgs<ShapefileMapLayer>(x => x.AreaColorPickerMenu);
        List<MenuItemViewModelBase> _areaColorPickerMenu;

        #endregion
    }
}
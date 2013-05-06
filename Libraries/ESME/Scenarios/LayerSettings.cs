using System;
using System.ComponentModel.DataAnnotations;
using System.Runtime.CompilerServices;
using System.Windows.Media;
using ESME.Database;
using ESME.Locations;
using ESME.Mapping;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;
using ThinkGeo.MapSuite.Core;

namespace ESME.Scenarios
{
    public class LayerSettings : ViewModelBase, IHaveGuid
    {
        public static Random Random = new Random();
        public LayerSettings()
        {
            PointSymbolType = new DbPointSymbolType {PointSymbolTypeAsInt = Random.Next(8)};
            Color curColor;
            while (true)
            {
                curColor = ColorExtensions.GetRandomNamedColor();
                var curYUV = LumaChromaColor.FromColor(curColor);
                if (curYUV.Y >= 0.5f) break;
            }
            LineOrSymbolDbColor = new DbColor(curColor);
            AreaDbColor = new DbColor(Colors.Transparent);
        }

        public LayerSettings(LayerSettings layerSettings)
        {
            IsChecked = layerSettings.IsChecked;
            PointSymbolType = layerSettings.PointSymbolType;
            LineOrSymbolDbColor = layerSettings.LineOrSymbolDbColor;
            AreaDbColor = layerSettings.AreaDbColor;
            LineOrSymbolSize = layerSettings.LineOrSymbolSize;
        }

        [Key, Initialize] public Guid Guid { get; set; }
        bool _isChecked;

        public bool IsChecked
        {
            get { return _isChecked; }
            set
            {
                _isChecked = value;
                if (DisplayIfScenarioIsLoadedFunc != null && !DisplayIfScenarioIsLoadedFunc())
                {
                    if (MapLayerViewModel != null) MediatorMessage.Send(MediatorMessage.HideMapLayer, MapLayerViewModel);
                    return;
                }
                if (MapLayerViewModel != null) MediatorMessage.Send(_isChecked ? MediatorMessage.ShowMapLayer : MediatorMessage.HideMapLayer, MapLayerViewModel);
            }
        }

        public DbPointSymbolType PointSymbolType { get; set; }
        public DbColor LineOrSymbolDbColor { get; set; }
        public DbColor AreaDbColor { get; set; }

        [Initialize(-1)]
        public int LayerOrder { get; set; }

        [Initialize(1.0)] 
        public double LineOrSymbolSize
        {
            get { return _lineOrSymbolSize; }
            set
            {
                _lineOrSymbolSize = value;
                if (MapLayerViewModel == null) return;
                MapLayerViewModel.LineWidth = (float)value;
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, MapLayerViewModel);
            }
        }
        double _lineOrSymbolSize;

        [NotMapped] public Func<bool> DisplayIfScenarioIsLoadedFunc { get; set; }
        [NotMapped] public PointSymbolType SymbolType
        {
            get { return _symbolType; }
            set
            {
                _symbolType = value;
                if (MapLayerViewModel == null) return;
                MapLayerViewModel.PointSymbolType = value;
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, MapLayerViewModel);
            }
        }
        PointSymbolType _symbolType;

        [NotMapped]
        public Color LineOrSymbolColor
        {
            get { return LineOrSymbolDbColor.Color; }
            set
            {
                LineOrSymbolDbColor.Color = value;
                if (MapLayerViewModel == null) return;
                MapLayerViewModel.LineColor = value;
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, MapLayerViewModel);
            }
        }

        [NotMapped]
        public Color AreaColor
        {
            get { return AreaDbColor.Color; }
            set
            {
                AreaDbColor.Color = value;
                if (MapLayerViewModel == null) return;
                MapLayerViewModel.AreaColor = value;
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, MapLayerViewModel);
            }
        }

        public void MoveLayerToFront() { if (_mapLayerViewModel != null) MediatorMessage.Send(MediatorMessage.MoveLayerToFront, _mapLayerViewModel); }
        public void MoveLayerForward() { if (_mapLayerViewModel != null) MediatorMessage.Send(MediatorMessage.MoveLayerForward, _mapLayerViewModel); }
        public void MoveLayerBackward() { if (_mapLayerViewModel != null) MediatorMessage.Send(MediatorMessage.MoveLayerBackward, _mapLayerViewModel); }
        public void MoveLayerToBack() { if (_mapLayerViewModel != null) MediatorMessage.Send(MediatorMessage.MoveLayerToBack, _mapLayerViewModel); }

        [NotMapped] 
        public MapLayerViewModel MapLayerViewModel
        {
            [MethodImpl(MethodImplOptions.Synchronized)]
            get { return _mapLayerViewModel; }
            [MethodImpl(MethodImplOptions.Synchronized)]
            set
            {
                if (value == null && _mapLayerViewModel != null) MediatorMessage.Send(MediatorMessage.RemoveMapLayer, _mapLayerViewModel);
                else if (_mapLayerViewModel == value)
                {
                    MediatorMessage.Send(MediatorMessage.RefreshMapLayer, _mapLayerViewModel);
                    return;
                }
                _mapLayerViewModel = value;
                if (_mapLayerViewModel == null) return;
                MediatorMessage.Send(MediatorMessage.AddMapLayer, _mapLayerViewModel);
                MediatorMessage.Send(IsChecked ? MediatorMessage.ShowMapLayer : MediatorMessage.HideMapLayer, _mapLayerViewModel);
                _mapLayerViewModel.AreaColor = AreaColor;
                _mapLayerViewModel.LineColor = LineOrSymbolColor;
                _mapLayerViewModel.PointSymbolType = SymbolType;
                _mapLayerViewModel.LineWidth = (float)LineOrSymbolSize;
            }
        }
        MapLayerViewModel _mapLayerViewModel;

        public void RefreshMapLayer()
        {
            if (_mapLayerViewModel == null) return;
            MediatorMessage.Send(MediatorMessage.RefreshMapLayer, _mapLayerViewModel);
        }
    }
}
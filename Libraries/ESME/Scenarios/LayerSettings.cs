using System;
using System.ComponentModel.DataAnnotations;
using System.Windows.Media;
using ESME.Database;
using ESME.Locations;
using HRC.Aspects;
using HRC.WPF;

namespace ESME.Scenarios
{
    [NotifyPropertyChanged]
    public class LayerSettings : IHaveGuid
    {
        public static Random _random = new Random();
        public LayerSettings()
        {
            IsChecked = true;
            PointSymbolType = new DbPointSymbolType {PointSymbolTypeAsInt = _random.Next(8)};
            LineColor = new DbColor(ColorExtensions.GetRandomNamedColor());
            LineWeight = 1f;
            AreaColor = new DbColor(ColorExtensions.GetRandomNamedColor());
            LayerOrder = -1;
        }
        [Key, Initialize]
        public Guid Guid { get; set; }
        public bool IsChecked { get; set; }
        public DbPointSymbolType PointSymbolType { get; set; }
        public DbColor LineColor { get; set; }
        public float LineWeight { get; set; }
        public DbColor AreaColor { get; set; }
        public int LayerOrder { get; set; }

        [NotMapped]
        public Color ActualLineColor
        {
            get { return LineColor.Color; }
            set { LineColor.Color = value; }
        }
    }
}
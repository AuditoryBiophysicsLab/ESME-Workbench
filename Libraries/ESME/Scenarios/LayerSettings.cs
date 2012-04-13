using System;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.Windows.Media;
using ESME.Database;
using ESME.Locations;
using HRC;
using HRC.Aspects;
using HRC.WPF;

namespace ESME.Scenarios
{
    [NotifyPropertyChanged]
    public class LayerSettings : IHaveGuid
    {
        public static Random Random = new Random();
        public LayerSettings()
        {
            IsChecked = true;
            PointSymbolType = new DbPointSymbolType {PointSymbolTypeAsInt = Random.Next(8)};
            LineOrSymbolDbColor = new DbColor(ColorExtensions.GetRandomNamedColor());
            LineOrSymbolSize = 1f;
            AreaDbColor = new DbColor(ColorExtensions.GetRandomNamedColor());
            LayerOrder = -1;
        }
        [Key, Initialize]
        public Guid Guid { get; set; }
        public bool IsChecked { get; set; }
        public DbPointSymbolType PointSymbolType { get; set; }
        public DbColor LineOrSymbolDbColor { get; set; }
        public double LineOrSymbolSize { get; set; }
        public DbColor AreaDbColor { get; set; }
        public int LayerOrder { get; set; }

        [NotMapped]
        public Color LineOrSymbolColor
        {
            get { return LineOrSymbolDbColor.Color; }
            set { LineOrSymbolDbColor.Color = value; }
        }
        [NotMapped]
        public Color AreaColor
        {
            get { return AreaDbColor.Color; }
            set { AreaDbColor.Color = value; }
        }
        public event PropertyChangedEventHandler PropertyChanged;
        protected void OnPropertyChanged(string propertyName)
        {
            if (PropertyChanged != null)
                PropertyChanged(this, new PropertyChangedEventArgs(propertyName));
        }
    }
}
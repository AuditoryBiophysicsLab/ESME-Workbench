using System;
using System.Windows.Media;
using ESME.Database;
using HRC.Aspects;
using HRC.ViewModels;
using HRC.WPF;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class SedimentTypeNode : ViewModelBase
    {
        public static Random Random = new Random();
        public SedimentTypeNode()
        {
            SymbolColor = new DbColor(ColorExtensions.GetRandomNamedColor());
            PointSymbolType = new DbPointSymbolType { PointSymbolTypeAsInt = Random.Next(8) };
        }

        public DbPointSymbolType PointSymbolType { get; set; }
        public Color SymbolColor { get; set; }
        [Initialize(1.0)]
        public double SymbolSize { get; set; }
        [Initialize(true)]
        public bool IsChecked { get; set; }
        public string SedimentType { get; set; }
    }
}
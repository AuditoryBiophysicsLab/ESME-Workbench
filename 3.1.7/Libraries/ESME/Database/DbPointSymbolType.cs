using System.ComponentModel.DataAnnotations;
using ThinkGeo.MapSuite.Core;

namespace ESME.Database
{
    [ComplexType]
    public class DbPointSymbolType
    {
        public DbPointSymbolType() { }
        public DbPointSymbolType(PointSymbolType pointSymbolType) { PointSymbolTypeAsInt = (byte)pointSymbolType; }
        public static implicit operator DbPointSymbolType(PointSymbolType pointSymbolType) { return new DbPointSymbolType(pointSymbolType); }
        public static implicit operator PointSymbolType(DbPointSymbolType dbPointSymbolType) { return (PointSymbolType)dbPointSymbolType.PointSymbolTypeAsInt; }
        public int PointSymbolTypeAsInt { get; set; }
    }
}
using ESME.Environment.NAVO;
using HRC;
using HRC.Navigation;

namespace ESME.Environment
{
    public interface IEnvironmentalDataSource<out T> : IHRCPlugin
    {
        float Resolution { get; }
        T Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod);
    }
}

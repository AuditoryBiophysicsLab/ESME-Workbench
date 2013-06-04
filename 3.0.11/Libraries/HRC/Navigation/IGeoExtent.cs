using HRC.Navigation;

namespace HRC.Navigation
{
    public interface IGeoExtent
    {
        /** compute a point and radius around the extent. */
        BoundingCircle BoundingCircle { get; }
    }
}

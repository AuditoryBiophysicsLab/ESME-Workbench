using ESME.Behaviors;

namespace ESME.Database
{
    public class DbTrackType
    {
        public DbTrackType() { }
        public DbTrackType(TrackType trackType) { TrackTypeAsByte = (byte)trackType; }
        public static implicit operator DbTrackType(TrackType trackType) { return new DbTrackType(trackType); }
        public static implicit operator TrackType(DbTrackType dbTrackType) { return (TrackType)dbTrackType.TrackTypeAsByte; }
        public byte TrackTypeAsByte { get; set; }
    }
}
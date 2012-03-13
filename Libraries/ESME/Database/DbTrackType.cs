using System.ComponentModel.DataAnnotations;

namespace ESME.Database
{
    [ComplexType]
    public class DbTrackType
    {
        public DbTrackType() { }
        public DbTrackType(TrackType trackType) { TrackTypeAsByte = (byte)trackType; }
        public static implicit operator DbTrackType(TrackType trackType) { return new DbTrackType(trackType); }
        public static implicit operator TrackType(DbTrackType dbTrackType) { return (TrackType)dbTrackType.TrackTypeAsByte; }
        public byte TrackTypeAsByte { get; set; }

        [NotMapped]
        public TrackType TrackType
        {
            get { return (TrackType)TrackTypeAsByte; }
            set { TrackTypeAsByte = (byte)value; }
        }
    }

    public enum TrackType
    {
        Stationary = 0,
        StraightLine = 1,
        PerimeterBounce = 2,
    }
}
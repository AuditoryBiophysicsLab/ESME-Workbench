using System;
using System.ComponentModel.DataAnnotations;
using System.IO;
using System.Linq;
using ESME.Database;
using ESME.Locations;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Scenarios
{
    [NotifyPropertyChanged]
    public class ShipTrack : IHaveGuid
    {
        public ShipTrack() { Initialize(null); }
        public ShipTrack(Platform platform) { Initialize(platform); }
        public ShipTrack(Platform platform, ShipTrack shipTrack)
        {
            Initialize(platform);
            Copy(shipTrack);
        }

        void Initialize(Platform platform)
        {
            if (platform != null) Platform = platform;
        }

        void Copy(ShipTrack shipTrack)
        {
            OverrideTimestamps = shipTrack.OverrideTimestamps;
            foreach (var waypoint in shipTrack.Waypoints)
                Waypoints.Add(new Waypoint(waypoint));
        }

        [Key, Initialize] public Guid Guid { get; set; }
        public bool OverrideTimestamps { get; set; }

        [Initialize] public virtual ObservableList<Waypoint> Waypoints { get; set; }

        public virtual Platform Platform { get; set; }
        [Initialize] public virtual ObservableList<LogEntry> Logs { get; set; }

        [NotMapped] public bool HasTimestamps { get; private set; }

        public void CheckTimestamps()
        {
            if (Waypoints == null || Waypoints.Count == 0) HasTimestamps = false;
            else HasTimestamps = Waypoints.Any(w => w.TimeAtWaypoint.Ticks > 0);
        }

        public static ShipTrack ReadWaypointFile(string filename)
        {
            var shipTrack = new ShipTrack();
            var lines = File.ReadAllLines(filename);
            char[] separators = {' ', ',', '\t'};
            var lineNumber = 0;
            var lastTimeSpan = TimeSpan.Zero;
            var hasTimestamps = false;
            shipTrack.Waypoints.Clear();
            var order = 0;
            foreach (var line in lines)
            {
                var timeSpan = TimeSpan.Zero;
                lineNumber++;
                var fields = line.Split(separators, StringSplitOptions.RemoveEmptyEntries);
                double latitude, longitude;
                if (fields.Length < 2) WaypointFormatException(lineNumber, "Each line should contain a minimum two fields (a latitude and a longitude)");
                if (fields.Length > 3)  WaypointFormatException(lineNumber, "Each line should contain a maximum of three fields (a latitude, a longitude and a time stamp)");
                if (!Double.TryParse(fields[0], out latitude)) WaypointFormatException(lineNumber, "Invalid number format for latitude value");
                if (!Double.TryParse(fields[1], out longitude)) WaypointFormatException(lineNumber, "Invalid number format for longitude value");
                if (fields.Length == 3 && !TimeSpan.TryParse(fields[2], out timeSpan)) WaypointFormatException(lineNumber, "Invalid TimeSpan format for time stamp value. Format is hh:mm:ss");
                if (latitude < -90.0 || latitude > 90.0) WaypointFormatException(lineNumber, "Invalid latitude value. Latitude must be between -90 and +90");
                if (longitude < -360.0 || longitude > 360.0) WaypointFormatException(lineNumber, "Invalid longitude value. Longitude must be between -360 and +360");
                if (timeSpan.Ticks < 0) WaypointFormatException(lineNumber, "Invalid time stamp value. Time stamp value cannot be negative");
                if (lineNumber == 1 && timeSpan.Ticks != 0) WaypointFormatException(lineNumber, "Initial postion time stamp is required to be 00:00:00 if present");
                if (timeSpan.Ticks > 0 && lineNumber > 2 && !hasTimestamps) WaypointFormatException(lineNumber, "A waypoint file that has any timestamps is required to have timestamps on every line");
                if (timeSpan.Ticks > 0) hasTimestamps = true;
                if (lastTimeSpan.Ticks > 0 && timeSpan.Ticks < lastTimeSpan.Ticks) WaypointFormatException(lineNumber, "Invalid time stamp value. Time stamp value must be greater than the time stamp value specified for the previous waypoint");
                shipTrack.Waypoints.Add(new Waypoint { Geo = new Geo(latitude, longitude), TimeAtWaypoint = timeSpan, ShipTrack = shipTrack, Order = order++ });
                lastTimeSpan = timeSpan;
            }
            shipTrack.CheckTimestamps();
            return shipTrack;
        }

        static void WaypointFormatException(int lineNumber, string message) { throw new WaypointFileFormatException(string.Format("Illegal waypoint file format at line {0}: {1}", lineNumber, message)); }

        public void WriteWaypointFile(string filename, bool includeTimestampsIfPresent)
        {
            if (string.IsNullOrEmpty(filename)) return;
            var includeTimestamps = includeTimestampsIfPresent && HasTimestamps;
            using (var writer = new StreamWriter(filename, false))
            {
                foreach (var waypoint in Waypoints)
                {
                    var geo = (Geo)waypoint.Geo;
                    writer.WriteLine(string.Format("{0} {1}{2}", geo.Latitude, geo.Longitude, includeTimestamps ? " " +((TimeSpan)waypoint.TimeAtWaypoint) : ""));
                }
            }
        }
    }

    public class Waypoint : IHaveGuid
    {
        public Waypoint() { }

        public Waypoint(Waypoint coordinate)
        {
            Geo = new Geo(coordinate.Geo);
            TimeAtWaypoint = new DbTimeSpan(coordinate.TimeAtWaypoint);
        }
        [Key, Initialize] public Guid Guid { get; set; }
        public DbGeo Geo { get; set; }
        /// <summary>
        /// The time offset from the start of the scenario that the platform arrives at this waypoint
        /// </summary>
        public DbTimeSpan TimeAtWaypoint { get; set; }
        public int Order { get; set; }
        public virtual ShipTrack ShipTrack { get; set; }
    }
}

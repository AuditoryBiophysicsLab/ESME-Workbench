using System;
using System.Collections.Generic;
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
            Waypoints.CollectionChanged += (s, e) =>
            {
                if (Waypoints == null || Waypoints.Count == 0) HasTimestamps = false;
                HasTimestamps = Waypoints.Any(w => w.TimeAtWaypoint.Ticks > 0);
            };
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

        public void ReadWaypointFile(string filename)
        {
            if (string.IsNullOrEmpty(filename) || !File.Exists(filename)) return;
            var lines = File.ReadAllLines(filename);
            char[] separators = {' ', ',', '\t'};
            var lineNumber = 0;
            var waypoints = new List<Waypoint>();
            var lastTimeSpan = TimeSpan.Zero;
            foreach (var line in lines)
            {
                var timeSpan = TimeSpan.Zero;
                lineNumber++;
                var fields = line.Split(separators, StringSplitOptions.RemoveEmptyEntries);
                double latitude, longitude;
                if (fields.Length < 2) throw new FormatException(string.Format("Illegal waypoint file format at line {0}: each line should contain a minimum two fields (a latitude and a longitude)", lineNumber));
                if (fields.Length > 3) throw new FormatException(string.Format("Illegal waypoint file format at line {0}: each line should contain a maximum of three fields (a latitude, a longitude and a time stamp)", lineNumber));
                if (!Double.TryParse(fields[0], out latitude)) throw new FormatException(string.Format("Illegal waypoint file format at line {0}: Invalid number format for latitude value", lineNumber));
                if (!Double.TryParse(fields[1], out longitude)) throw new FormatException(string.Format("Illegal waypoint file format at line {0}: Invalid number format for longitude value", lineNumber));
                if (fields.Length == 3 && !TimeSpan.TryParse(fields[2], out timeSpan)) throw new FormatException(string.Format("Illegal waypoint file format at line {0}: Invalid TimeSpan format for time stamp value. Format is hh:mm:ss", lineNumber));
                if (latitude < -90.0 || latitude > 90.0) throw new FormatException(string.Format("Illegal waypoint file format at line {0}: Invalid latitude value. Latitude must be between -90 and +90", lineNumber));
                if (longitude < -360.0 || longitude > 360.0) throw new FormatException(string.Format("Illegal waypoint file format at line {0}: Invalid longitude value. Longitude must be between -360 and +360", lineNumber));
                if (timeSpan.Ticks < 0) throw new FormatException(string.Format("Illegal waypoint file format at line {0}: Invalid time stamp value. Time stamp value cannot be negative", lineNumber));
                if (lastTimeSpan.Ticks > 0 && timeSpan.Ticks < lastTimeSpan.Ticks) throw new FormatException(string.Format("Illegal waypoint file format at line {0}: Invalid time stamp value. Time stamp value must be greater than the time stamp value specified for the previous waypoint", lineNumber));
                waypoints.Add(new Waypoint { Geo = new Geo(latitude, longitude), TimeAtWaypoint = timeSpan, ShipTrack = this });
                lastTimeSpan = timeSpan;
            }
            Waypoints.Clear();
            Waypoints.AddRange(waypoints);
        }

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

        public virtual ShipTrack ShipTrack { get; set; }
    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using HRC.Navigation;

namespace ESME.ShipTrack
{
    class Generator
    {
#if false
        private Random myRandom = new Random((int)DateTime.Now.Ticks);
        private Bearing Bearing, Mowing, Moving;
        private enum BoundsCrossed { North, South, East, West, Northwest, Northeast, Southwest, Southeast, None };
        private enum LawnmowerState { Mowing, Moving };
        private LawnmowerState CurState = LawnmowerState.Mowing;
        private const double KnotsToMetersPerSecond = 0.514444444444444;

        private void CreateShipTrack(EarthCoordinate StartPosition)
        {
            TimeSpan SimTime = new TimeSpan();
            TimeSpan SimIncrement = new TimeSpan(0, 1, 0);
            int MinutesElapsed = 0;
            EarthCoordinate CurPosition, NewPosition;
            double BeamLookDirection, BeamHalfWidth;
            double Distance_Meters;
            BoundsCrossed whichBorder;
            int MovingCount = 0;
            bool WriteWaypoint = true;

            CurPosition = new EarthCoordinate(StartPosition);
            BeamLookDirection = Bearing.Degrees;
            BeamHalfWidth = 180;
            using (StreamWriter TrackFile = new StreamWriter(Properties.Settings.Default.TrackFile))
            {
                while (SimTime < Properties.Settings.Default.TrackDuration)
                {
                    if (WriteWaypoint)
                    {
                        BeamLookDirection = Bearing.Degrees;
                        BeamHalfWidth = 180;

                        TrackFile.WriteLine("{0:00}{1:00} {2:##0.####} {3:###0.####} {4:##0} {5:##0}", SimTime.Hours, SimTime.Minutes % 60, CurPosition.Latitude, CurPosition.Longitude, BeamLookDirection, BeamHalfWidth);
                        MinutesElapsed = 0;
                        WriteWaypoint = false;
                    }

                    Distance_Meters = Properties.Settings.Default.Speed_knots * KnotsToMetersPerSecond * 60;    // Distance traveled by the ship in one minute
                    NewPosition = EarthCoordinate.BearingAndDistanceFrom(CurPosition, Bearing.Degrees, Distance_Meters);
                    whichBorder = TryMove(NewPosition);
                    if ((Properties.Settings.Default.CourseType == 1) && (CurState == LawnmowerState.Moving))  // If we're mowing the lawn AND we're in the Moving state
                    {
                        MovingCount--;
                        if (MovingCount == 0)
                        {
                            // We're transitioning from Moving to Mowing, so drop a waypoint
                            WriteWaypoint = true;
                            Bearing = Mowing;
                            CurState = LawnmowerState.Mowing;
                        }
                    }
                    if (whichBorder != BoundsCrossed.None)
                    {
                        // We would have crossed the border if we maintained current course and speed, so we want to bounce off the border in some fashion
                        // Since we're going to change course for the current minute, we need to drop a waypoint here
                        WriteWaypoint = true;
                        switch (Properties.Settings.Default.CourseType)
                        {
                            case 0: // Random initial course, then normal reflections off the op area borders
                                switch (whichBorder)
                                {
                                    case BoundsCrossed.North:
                                        Bearing.Reflect(180.0);
                                        break;
                                    case BoundsCrossed.South:
                                        Bearing.Reflect(0.0);
                                        break;
                                    case BoundsCrossed.East:
                                        Bearing.Reflect(270.0);
                                        break;
                                    case BoundsCrossed.West:
                                        Bearing.Reflect(90.0);
                                        break;
                                    case BoundsCrossed.Northwest:
                                        Bearing.Reflect(135.0);
                                        break;
                                    case BoundsCrossed.Northeast:
                                        Bearing.Reflect(225.0);
                                        break;
                                    case BoundsCrossed.Southwest:
                                        Bearing.Reflect(45.0);
                                        break;
                                    case BoundsCrossed.Southeast:
                                        Bearing.Reflect(315.0);
                                        break;
                                }
                                break;
                            case 1: // Lawnmower
                                switch (CurState)
                                {
                                    case LawnmowerState.Mowing:
                                        Mowing.Degrees = Mowing.ReciprocalDegrees;
                                        Bearing = Moving;
                                        CurState = LawnmowerState.Moving;
                                        MovingCount = 11;
                                        break;
                                    case LawnmowerState.Moving:
                                        // Only when we hit a wall while Moving do we reverse the direction in which we move
                                        Moving.Degrees = Moving.ReciprocalDegrees;
                                        Bearing = Moving;
                                        MovingCount = 11;
                                        break;
                                }
                                break;
                            default:
                                return;
                        }
                    }   // If we would have crossed a border during this move interval
                    if (!WriteWaypoint)
                    {
                        CurPosition = NewPosition;  // Update the current position
                        SimTime = SimTime.Add(SimIncrement);
                        MinutesElapsed++;
                        if (MinutesElapsed >= 30)
                            WriteWaypoint = true;
                    }
                } // While there's still time left to move the ship
                // When we're done moving, write a final waypoint
                TrackFile.WriteLine("{0:00}{1:00} {2:##0.####} {3:###0.####} {4:##0} {5:##0}", SimTime.Hours, SimTime.Minutes % 60, CurPosition.Latitude, CurPosition.Longitude, BeamLookDirection, BeamHalfWidth);
            }
            MessageBox.Show("If you have ESME Workbench open, and a ship track with the same name did not previously appear in the open experiment, you may need to reload the experiment in order to see the new ship track.  If the ship track was simply regenerated with the same name previously appeared, simply switching back to ESME Workbench will cause the new track to be loaded", "Ship track generated successfully", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }

        private BoundsCrossed TryMove(EarthCoordinate NewLocation)
        {
            if (NewLocation.Latitude > Properties.Settings.Default.NWLat)
            {
                // Crossed northern border, check east and west also
                if (NewLocation.Longitude < Properties.Settings.Default.NWLong)
                    return BoundsCrossed.Northwest;     // Crossed western border also
                if (NewLocation.Longitude > Properties.Settings.Default.SELong)
                    return BoundsCrossed.Northeast;     // Crossed eastern border also
                return BoundsCrossed.North;
            }
            if (NewLocation.Latitude < Properties.Settings.Default.SELat)
            {
                // Crossed southern border, check east and west also
                if (NewLocation.Longitude < Properties.Settings.Default.NWLong)
                    return BoundsCrossed.Southwest;     // Crossed western border also
                if (NewLocation.Longitude > Properties.Settings.Default.SELong)
                    return BoundsCrossed.Southeast;     // Crossed eastern border also
                return BoundsCrossed.South;
            }
            if (NewLocation.Longitude < Properties.Settings.Default.NWLong)
                return BoundsCrossed.West;              // Crossed western border
            if (NewLocation.Longitude > Properties.Settings.Default.SELong)
                return BoundsCrossed.East;               // Crossed eastern border
            return BoundsCrossed.None;                   // Still within the OpArea
        }
#endif
    }
}

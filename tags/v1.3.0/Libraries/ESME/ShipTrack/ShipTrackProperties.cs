using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME.ShipTrack
{
    public class ShipTrackProperties
    {
#if false
        private string shipName;
        private int[] timeVector;
        private int[] secondsVector;
        private double[] latitudeVector;
        private double[] longitudeVector;
        private int[] headingVector;
        private int[] beamVector;
        private SoundSource[] soundSourceVector;
        public bool TLisComputed;
        private string fullFileName;
        private DateTime modificationTime;
        private Experiment curExperiment;

        public ShipTrackProperties(string shipName, ref Experiment curExperiment, int[] timeVector, double[] latitudeVector, double[] longitudeVector, int[] headingVector, int[] beamVector,
                                   double[] frequencyVector, double[] intensityVector, double[] depthVector, double[] transectLengthVector,
                                   double[] verticalAngleVector, double[] verticalHalfAngleVector, double[] pingIntervalVector, double[] pingDurationVector,
                                   int[] transectCountVector, string FullFileName, DateTime ModificationTime)
        {

            this.shipName = shipName;
            this.timeVector = timeVector;
            this.latitudeVector = latitudeVector;
            this.longitudeVector = longitudeVector;
            this.headingVector = headingVector;
            this.beamVector = beamVector;
            soundSourceVector = new SoundSource[timeVector.Length];
            secondsVector = new int[timeVector.Length];
            TLisComputed = true;
            fullFileName = FullFileName;
            modificationTime = ModificationTime;
            this.curExperiment = curExperiment;

            for (int i = 0; i < timeVector.Length; i++)
            {
                int hours, minutes;

                minutes = timeVector[i] % 100;
                hours = timeVector[i] / 100;
                secondsVector[i] = (hours * 3600) + (minutes * 60);
                soundSourceVector[i] = new SoundSource(shipName + "-t" + timeVector[i].ToString("0000"), curExperiment, frequencyVector[i], intensityVector[i],
                                                       depthVector[i], new EarthCoordinate(latitudeVector[i], longitudeVector[i]), headingVector[i],
                                                       transectLengthVector[i], beamVector[i], verticalAngleVector[i], verticalHalfAngleVector[i],
                                                       transectCountVector[i], pingIntervalVector[i], pingDurationVector[i]);
                if ((beamVector[i] != 0) && (!soundSourceVector[i].SaveFileIsPresent))
                    TLisComputed = false;
            }
        }

        private void CheckForUpdate()
        {
            FileInfo fileInfo = new FileInfo(fullFileName);
            ShipTrackProperties newTrack;
            if (fileInfo.LastWriteTime > modificationTime)
            {
                newTrack = ReaderShipTrack.Read(fileInfo, curExperiment);
                CopyFrom(newTrack);
            }
        }

        private void CopyFrom(ShipTrackProperties newTrack)
        {
            this.shipName = newTrack.shipName;
            this.timeVector = newTrack.timeVector;
            this.latitudeVector = newTrack.latitudeVector;
            this.longitudeVector = newTrack.longitudeVector;
            this.headingVector = newTrack.headingVector;
            this.beamVector = newTrack.beamVector;
            this.soundSourceVector = newTrack.soundSourceVector;
            this.TLisComputed = newTrack.TLisComputed;
            this.fullFileName = newTrack.fullFileName;
            this.modificationTime = newTrack.modificationTime;
            this.curExperiment = newTrack.curExperiment;
            this.secondsVector = newTrack.secondsVector;
            this.soundSourceVector = newTrack.soundSourceVector;
            this.beamVector = newTrack.beamVector;
        }

        public string ShipName { get { CheckForUpdate(); return this.shipName; } }
        public int[] TimeVector { get { CheckForUpdate(); return this.timeVector; } }
        public double[] LatitudeVector { get { CheckForUpdate(); return this.latitudeVector; } }
        public double[] LongitudeVector { get { CheckForUpdate(); return this.longitudeVector; } }
        public int[] HeadingVector { get { CheckForUpdate(); return this.headingVector; } }
        public int[] BeamVector { get { CheckForUpdate(); return this.beamVector; } }
        public SoundSource[] SoundSourceVector { get { CheckForUpdate(); return this.soundSourceVector; } }

        public EarthCoordinate PositionAtTime(int Seconds)
        {
            if ((Seconds < 0) | (Seconds >= secondsVector[secondsVector.Length - 1]))
                return null;
            for (int waypoint = 0; waypoint < (secondsVector.Length - 1); waypoint++)
            {
                // First, find out what waypoint is the source for the time in question
                if ((secondsVector[waypoint] <= Seconds) && (secondsVector[waypoint + 1] > Seconds))
                {
                    // OK, the current waypoint is the source waypoint for the position at the requested time
                    int startTime, endTime;
                    double startLat, startLong, endLat, endLong, curLat, curLong, deltaPercent;

                    startTime = this.secondsVector[waypoint];
                    endTime = this.secondsVector[waypoint + 1];
                    startLat = this.latitudeVector[waypoint];
                    startLong = this.longitudeVector[waypoint];
                    endLat = this.latitudeVector[waypoint + 1];
                    endLong = this.longitudeVector[waypoint + 1];

                    // calculate the percentage of the current course segment that given time, Seconds, accounts for
                    deltaPercent = ((double)(Seconds - startTime)) / ((double)(endTime - startTime));
                    curLat = startLat + ((endLat - startLat) * deltaPercent);
                    curLong = startLong + ((endLong - startLong) * deltaPercent);
                    return new EarthCoordinate(curLat, curLong);
                }
            }
            // this should never ever happen
            return null;
            //throw new ApplicationException("Bizarre exception looking up location for moving source \"" + this.shipName + "\" at time " + Seconds + " seconds");
        }

        public SoundSource SourceAtTime(int Seconds)
        {
            EarthCoordinate curLocation = PositionAtTime(Seconds);

            if (curLocation == null)
                return null;
            for (int waypoint = 0; waypoint < (secondsVector.Length - 1); waypoint++)
            {
                // First, find out what waypoint is the source for the time in question
                if ((secondsVector[waypoint] <= Seconds) && (secondsVector[waypoint + 1] > Seconds))
                {
                    if (beamVector[waypoint] == 0)
                        return null;
                    else
                    {
                        soundSourceVector[waypoint].CenterCoordinate = curLocation;
                        return soundSourceVector[waypoint];
                    }
                }
            }
            return null;
        }
#endif
    }
}

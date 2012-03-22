using System.IO;
using ESME.Scenarios;

namespace ESME.Simulator
{
    public class ActorExposureRecord
    {
        public int SourceActorModeID { get; private set; }
        public float PeakSPL { get; private set; }
        public float Energy { get; private set; }

        public Mode Mode { get; private set; }

        ActorExposureRecord() {}

        public ActorExposureRecord(int sourceActorModeID, float peakSPL, float energy) 
        {
            SourceActorModeID = sourceActorModeID;
            PeakSPL = peakSPL;
            Energy = energy;
        }

        internal static ActorExposureRecord Read(BinaryReader reader)
        {
            return new ActorExposureRecord
            {
                SourceActorModeID = reader.ReadInt32(),
                PeakSPL = reader.ReadSingle(),
                Energy = reader.ReadSingle(),
            };
        }

        internal void Write(BinaryWriter writer)
        {
            writer.Write(SourceActorModeID);
            writer.Write(PeakSPL);
            writer.Write(Energy);
        }
    }
}
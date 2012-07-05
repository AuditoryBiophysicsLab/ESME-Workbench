using System.IO;
using ESME.Scenarios;

namespace ESME.Simulator
{
    public class ActorExposureRecord
    {
        public int ActorID { get; private set; }
        public int SourceActorModeID { get; private set; }
        public float PeakSPL { get; private set; }
        public float Energy { get; private set; }
        public Mode Mode { get; private set; }

        ActorExposureRecord() {}

        public ActorExposureRecord(int actorID, Mode mode, float peakSPL, float energy)
        {
            ActorID = actorID;
            Mode = mode;
            SourceActorModeID = Mode.SourceActorModeID;
            PeakSPL = peakSPL;
            Energy = energy;
        }

        internal static ActorExposureRecord Read(BinaryReader reader)
        {
            return new ActorExposureRecord
            {
                ActorID = reader.ReadInt32(),
                SourceActorModeID = reader.ReadInt32(),
                PeakSPL = reader.ReadSingle(),
                Energy = reader.ReadSingle(),
            };
        }

        internal void Write(BinaryWriter writer)
        {
            writer.Write(ActorID);
            writer.Write(SourceActorModeID);
            writer.Write(PeakSPL);
            writer.Write(Energy);
        }
    }
}
namespace ESME.Simulator
{
    public interface ITimeStepProcessor
    {
        SimulationLog SimulationLog { get; }
        void Process(SimulationTimeStepRecord record);
    }
}

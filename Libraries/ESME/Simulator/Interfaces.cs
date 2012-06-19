namespace ESME.Simulator
{
    public interface ITimeStepProcessor
    {
        void Process(SimulationTimeStepRecord record);
        void Initialize(Simulation simulation);
    }
}

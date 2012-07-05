namespace ESME.Simulator
{
    public interface ITimeStepProcessor
    {
        Simulation Simulation { get; set; }
        void Process(SimulationTimeStepRecord record);
    }
}

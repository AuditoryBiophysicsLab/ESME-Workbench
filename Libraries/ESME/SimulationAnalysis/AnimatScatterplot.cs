using System.Collections.Generic;
using System.Linq;
using ESME.Simulator;
using FileHelpers;

namespace ESME.SimulationAnalysis
{
    public class AnimatScatterplot:ITimeStepProcessor
    {
        private Simulation _simulation;
        public Dictionary<int,int> Scatterplot ;
        
        public void Process(SimulationTimeStepRecord record)
        {
            var animats = from a in _simulation.GetActors()
                          where a.AnimatLocation != null && record.ActorPositionRecords[a.ID] !=null
                          select a;

            // for each animat in the simulation, tally their indidual exposure from each unique source.
            //foreach (var animat in from animat in animats from exposure in record.ActorPositionRecords[animat.ID].Exposures where exposure.PeakSPL > 120 select animat) //wat.
            foreach (var animat in animats)
            {
                foreach (var exposure in record.ActorPositionRecords[animat.ID].Exposures)
                {
                    if(exposure.PeakSPL > 120)
                    {
                        if(Scatterplot.ContainsKey(animat.ID)) Scatterplot[animat.ID] ++;
                        else Scatterplot.Add(animat.ID,1);
                    }   
                }
            }
        }

        public void Initialize(Simulation simulation)
        {
            _simulation = simulation;
            Scatterplot = new Dictionary<int, int>();
        }

        public void Serialize (string outFile)
        {
            var engine = new FileHelperEngine<AnimatScatterplotRecord>();
            var records = new List<AnimatScatterplotRecord>();
            var scatterplot = Scatterplot.ToList();
            for (var i = 0; i < scatterplot.Count; i++)
            {
                records.Add(new AnimatScatterplotRecord
                                {
                                    AnimatID = scatterplot[i].Key,
                                    ExposureCount = scatterplot[i].Value,
                                });

            }
            engine.WriteFile(outFile,records);
        }
    }
}

[DelimitedRecord(",")]
public class AnimatScatterplotRecord
{
    public int AnimatID;
    public int ExposureCount;
}
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Threading;
using System.Diagnostics;
using mbs; // C++ wrapper class code and data types
using MMMBSLib; // C# code code and data types.

namespace ESMECodeSnippit
{
    class Program
    {
        static void Main(string[] args)
        {
            C3mbs _mmmbs = new C3mbs();
            string animatScenarioFile = "jaxdolphin.sce";

            animatScenarioFile =  "jax3species.sce";
            mbsRESULT mbsResult;
            mbsCONFIG config = _mmmbs.GetConfiguration();
            mbsRUNSTATE runState;

           //load the .sce file
           if (mbsRESULT.OK != (mbsResult = _mmmbs.LoadScenario(animatScenarioFile)))
               Console.WriteLine("LoadScenario Error:" + _mmmbs.MbsResultToString(mbsResult));
           //make sure we're in durationless mode.
           config.durationLess = true;
           _mmmbs.SetConfiguration(config);

           //get species count
           int speciesCount = _mmmbs.GetSpeciesCount();

           //make a species list.
#if false
           SpeciesList = new SpeciesList();
           for (var i = 0; i <= speciesCount; i++)
           {
               SpeciesList.Add(new Species{SpeciesName = _mmmbs.GetSpeciesDisplayTitle(i),
                                   ReferenceCount = _mmmbs.GetIndivdualCount(i),});
           }
#endif
           //set up the position array from the values in the .sce file (not the ones in animatList, which doesn't exist yet..)
           int animatCount = _mmmbs.GetAnimatCount();
           var posArray = new mbsPosition[animatCount];

           //initialize the run, and wait until it's fully initialized.
           if(mbsRESULT.OK != (mbsResult = _mmmbs.InitializeRun()))
                Console.WriteLine("InitializeRun Error:" + _mmmbs.MbsResultToString(mbsResult));

           while ((runState = _mmmbs.GetRunState()) == mbsRUNSTATE.INITIALIZING)
           {
               //wait until initializing is done.
               Thread.Sleep(1);
           }

           //get the initial positions of every animat
           if(mbsRESULT.OK != (mbsResult = _mmmbs.GetAnimatCoordinates(posArray)))
               Console.WriteLine("Error Fetching Initial Animat Coordinates: " + _mmmbs.MbsResultToString(mbsResult));



           //bump the positions once, otherwise depths aren't set.
           if(mbsRESULT.OK != (mbsResult = _mmmbs.RunScenarioNumIterations(1)))
            Console.WriteLine("RunScenario Error:" + _mmmbs.MbsResultToString(mbsResult));

           //get the initial positions of every animat
           if (mbsRESULT.OK != (mbsResult = _mmmbs.GetAnimatCoordinates(posArray)))
            Console.WriteLine("Error Fetching Initial Animat Coordinates: " + _mmmbs.MbsResultToString(mbsResult));
        }
    }
}

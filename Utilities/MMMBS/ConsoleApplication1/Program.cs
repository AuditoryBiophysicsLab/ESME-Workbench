using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using MMMBSLib; // C# code code and data types.
using System.Threading.Tasks;
using System.Threading;
using System.Diagnostics;
using mbs; // C++ wrapper class code and data types


/* Modifications
 * 10/14/2010: 3MB library now rejects animat seeding on invalid locations such as land
 *             and species-specific bathymetry depths. A check is made for each attempt
 *             to seed an animat.
 *             
 * 10/14/2010: The function SeedingCoordinateIsValid() is supplied and shown below as part
 *             of this demo.  Ths function allows ESME to verify that a specific lat/lon
 *             coordinate is valid before attempting to actually seed an animat.
 *             Note that not all species necessarily have the same bathymetry depth
 *             requirements so a valid location for one species may be invalid at another.
 *             
 * note:       Check the returned mbsRESULT for all animat adds (via add functions such as
 *             AddIndividualAnimat(), AddIndividualAnimats(), AddPod(), and
 *             AddPodAnimats() to verify an add was successfull.  Successfull adds return
 *             an mbsRESULT.OK.
 *             
 * 10/14/2010: Added calls ClearScenario() when demo termintes on error or normally.
 *             
 * 10/14/2010: Moved the instantiation of each 3MB instance into a separate loop.
 *             This way ClearScenario() isn't called on uninstantiated 3mb instance in the
 *             initialization loop should an error occur.
 */



namespace _3MBParallelTest
{
    class Program
    {
        static void Main(string[] args)
        {
            int i;
            int j;
            int nNumInstances = 1;              // Number of 3mb processes to run in parallel.
            int numAnimatsPerInstance = 1000;  // Number of animats in each 3mb instance.
            uint duration = (uint)(5 * 3600);        // Number of simulated seconds 3mb is to run.
            int mmbSetDuration = (int)0;        // Number of simulated seconds 3mb is to run.
            uint numIterations = duration+1; ;  // The number of iterations is always the duration +1.
            string speFileName = "generic_mysticete.spe"; // demo species to use.
            Boolean feedback = true;

            C3mbs[] mmmbs;
            mbsPosition mbInitialPosition; // Animt lat/lon.  Used here to set and retrieve animat population lat/lon.
            mbsPosition[][] mbStepPosition; // Animt lat/lon.  Used here to set and retrieve animat population lat/lon.
            mbsRESULT mbResult = mbsRESULT.OK; // 3mb function call result.
            mbsCONFIG mbConfig; // various configuration parameters for 3mb.
            mbsRUNSTATE mbRunState;
            string sz;

            // Initialize each 3mb instance
            mmmbs = new C3mbs[nNumInstances];
            mbStepPosition = new mbsPosition[nNumInstances][];



            //--------------------------------------------------------------------------//
            // Instantiate 3mb loop.
            //-----------------------//
            for(i = 0; i < mmmbs.Length && mbsRESULT.OK == mbResult; i++)
                mmmbs[i] = new C3mbs();


            //--------------------------------------------------------------------------//
            // Initialize 3mb loop.
            //-----------------------//
            for(i = 0; i < mmmbs.Length && mbsRESULT.OK == mbResult; i++)
            {
                //----------------------------------------------------------------------//
                // Configure the scenario
                //------------------------//
                mbConfig = mmmbs[i].GetConfiguration();
                // Probably wise to set each 3mb instance to a unique randomizer seed value.
                mbConfig.seedValue = (uint)i;

                // ESME should set enabled to false and durationless to TRUE.
                mbConfig.enabled = false; // binary output enabled/disabled
                mbConfig.durationLess = true; // ESME runs 3mb in a durationless mode.
                mmmbs[i].SetConfiguration(mbConfig);

                mmmbs[i].SetDuration(mmbSetDuration);


                //----------------------------------------------------------------------//
                // Load bathymetry file
                //----------------------//
                /*  use either:
                 *      LoadBathymetryFromTextFile() if it has a .txt extension or
                 *      LoadFromBinFile() if the file has a .bth extension.
                 * 
                 * Or for development and testing it is OK to instead use:
                 *      mmmbs[i].SetBaythyConstantDepth();
                 *  that sets a constant depth throughout.
                 */
                mbResult = mmmbs[i].LoadBathymetryFromTextFile("Bahamas.txt");
                if(mbsRESULT.OK != mbResult)
                {
                    Console.WriteLine("\nError Loading Bathymetry File: " + mmmbs[i].MbsResultToString(mbResult));
                    foreach(C3mbs mmb in mmmbs)
                        mmb.ClearScenario(); // Deallocates memory, closes files, resets 3MB.
                    return;
                }


                //----------------------------------------------------------------------//
                // Add a species to the scenario
                //-------------------------------//
                mbResult = mmmbs[i].AddSpecies(Directory.GetCurrentDirectory() + "\\" + speFileName);
                if(mbsRESULT.OK != mbResult)
                {
                    Console.WriteLine("\nError species(" + speFileName +"), instance(" + i +"): " + mmmbs[i].MbsResultToString(mbResult));
                    foreach(C3mbs mmb in mmmbs)
                        mmb.ClearScenario(); // Deallocates memory, closes files, resets 3MB.
                    return;
                }

                // Set the scenario's duration.
                // (ESME runs in durationless mode so there's no need to set a duration).
                //mmmbs[i].SetDuration((int)duration);

                //----------------------------------------------------------------------//
                // Add animats to the species
                //----------------------------//
                // Faking an initial location for each animat on the Bahamas map for this demo
                mbInitialPosition.latitude = 26.193023;     // Actual latitude value on the Bahamas map
                mbInitialPosition.longitude = -78.251953;   // Actual longitude value on the Bahamas map
                mbInitialPosition.depth = 0;                // You can't actually set the animat's initial depth
                for(j=0; j<numAnimatsPerInstance; j++)
                {
                    // Providing a little feedback every 100 animats.
                    if(j%100 == 0)
                    {
                        sz = string.Format(
                            "Adding Animats: 3mb instance {0:0}/{1:0} animat {2:000}/{3:000}\r",
                            i+1,
                            mmmbs.Length,
                            j+1,
                            numAnimatsPerInstance);
                        Console.Write(sz);
                    }

                    // Added
                    mbResult = mmmbs[i].SeedingCoordinateIsValid((int)0, mbInitialPosition);
                    if(mbsRESULT.OK != mbResult)
                    {
                        Console.WriteLine("\nError with animat(" + j +"), initial coordinate, instance (" + i +"): " + mmmbs[i].MbsResultToString(mbResult));
                        foreach(C3mbs mmb in mmmbs)
                            mmb.ClearScenario(); // Deallocates memory, closes files, resets 3MB.
                        return;
                    }

                    // First param (0) is the species index.  This demo only loads a single species.
                    mbResult = mmmbs[i].AddIndividualAnimat(0, mbInitialPosition);
                    if(mbsRESULT.OK != mbResult)
                    {
                        Console.WriteLine("\nError adding animat(" + j +"), instance(" + i +"): " + mmmbs[i].MbsResultToString(mbResult));
                        foreach(C3mbs mmb in mmmbs)
                            mmb.ClearScenario(); // Deallocates memory, closes files, resets 3MB.
                        return;
                    }
                }
                sz = string.Format(
                    "Adding Animats: 3mb instance {0:0}/{1:0} animat {2:000}/{3:000}",
                    i+1,
                    mmmbs.Length,
                    j,
                    numAnimatsPerInstance);
                Console.WriteLine(sz);

                //----------------------------------------------------------------------//
                // Initialize the scenario.
                //----------------------------//
                mbResult = mmmbs[i].InitializeRun();
                if(mbsRESULT.OK == mbResult)
                {
                    // Wait for reach 3mb instance to finish initialzing (run state won't
                    // be mbsRUNSTATE.INITIALIZING) before initializing the next instance.
                    do
                    {
                        Thread.Sleep(50);
                        mbRunState = mmmbs[i].GetRunState();
                    } while(mbsRUNSTATE.INITIALIZING == mbRunState);
                }
                else
                {
                    Console.WriteLine("\nCritical Error Initializing 3mbs: " + mmmbs[i].MbsResultToString(mbResult));
                    return;
                }

                // Allocate space for this 3mb intance's animat positional data.
                mbStepPosition[i] = new mbsPosition[numAnimatsPerInstance];


                // Get Initial animat data.
                mbResult = mmmbs[i].GetAnimatCoordinates(mbStepPosition[i]);

                sz = string.Format(
                    "Initial Location Proc ({1}/{2}) animat({3}/{4}):  [{5,11:0.0000000}, {6,11:0.0000000}, {7,7:0.00}]",
                    i,                                // 0: step
                    j+1,                              // 1: process
                    nNumInstances,                    // 2: number of proceses
                    0+1,                              // 3: animat number (animat index + 1)
                    numAnimatsPerInstance,            // 4: number of animats (per 3mb instance)
                    mbStepPosition[i][0].latitude,    // 5: lat of animat at index 0
                    mbStepPosition[i][0].longitude,   // 6: lon of animat at index 0
                    mbStepPosition[i][0].depth);      // 7: depth of animat at index 0
                Console.WriteLine(sz);



            }



            //--------------------------------------------------------------------------//
            // Step through the 3mb instances
            //--------------------------------//
            for(i=0; i<duration && mbResult == mbsRESULT.OK; i++)
            {
                // Step the instances of 3mb in parallel.
                mbResult = Step3MB(mmmbs, i);
                if(mbsRESULT.OK != mbResult)
                    return;

                if(true == feedback)
                {
                    Console.Clear();
                }
                else
                {
                    if((i+1)%10 == 0)
                    {
                        sz = string.Format("iteration {0} of {1}\r", i+1, duration);
                        Console.Write(sz);
                    }
                }

                // Retrieve the animat position after each step (and do something with it).
                for(j=0; j<mmmbs.Length && mbsRESULT.OK == mbResult; j++)
                {
                    mbResult = mmmbs[j].GetAnimatCoordinates(mbStepPosition[j]);
                    if(mbsRESULT.OK != mbResult)
                    {
                        Console.WriteLine("Problem reading animat position data on 3mb instance " + i);
                    }
                    else if(true == feedback)
                    {
                        // Print out the first animat (index 0) of each instance every 100 iterations.
                        sz = string.Format(
                            "Step({0}) Proc ({1}/{2}) animat({3}/{4}):  [{5,11:0.0000000}, {6,11:0.0000000}, {7,7:0.00}]",
                            i,                                // 0: step
                            j+1,                              // 1: process
                            nNumInstances,                    // 2: number of proceses
                            0+1,                              // 3: animat number (animat index + 1)
                            numAnimatsPerInstance,            // 4: number of animats (per 3mb instance)
                            mbStepPosition[j][0].latitude,    // 5: lat of animat at index 0
                            mbStepPosition[j][0].longitude,   // 6: lon of animat at index 0
                            mbStepPosition[j][0].depth);      // 7: depth of animat at index 0
                        Console.WriteLine(sz);
                    }

                }
            }

            //--------------------------------------------------------------------------//
            // Considerately tell the instances of 3mb to shut down
            //------------------------------------------------------//
            for(i = 0; i < mmmbs.Length && mbsRESULT.OK == mbResult; i++)
                mmmbs[i].AbortRun();

            //--------------------------------------------------------------------------//
            // Verify all 3mb instances have shut down
            //-----------------------------------------//
            for(i = 0; i < mmmbs.Length && mbsRESULT.OK == mbResult; i++)
            {
                do
                {
                    Thread.Sleep(5);
                    mbRunState = mmmbs[i].GetRunState();
                } while(mbsRUNSTATE.FINISHED != mbRunState);
            }

            // Can't hurt to do a clear scenario...
            foreach(C3mbs mmb in mmmbs)
                mmb.ClearScenario(); // Deallocates memory, closes files, resets 3MB.


        }





        public static mbsRESULT Step3MB(C3mbs[] mbsArray, int CallNum)
        {
            int i = 0;
            mbsRESULT mbResult = mbsRESULT.OK; // 3mb function call result.
            mbsRUNSTATE mbRunState;

            i=0;
            Parallel.ForEach(mbsArray, curMbs =>
            {
                mbResult = curMbs.RunScenarioNumIterations(1);
                if(mbResult != mbsRESULT.OK)
                    Console.WriteLine("Failed on instance " + i + ", call " + CallNum + ": " + curMbs.MbsResultToString(mbResult));
                i++;
            });


            // Wait on all 3mb instances to finish the current step.
            for(i=0; i<mbsArray.Length; i++)
            {
                do
                {
                    Thread.Sleep(2);
                    mbRunState = mbsArray[i].GetRunState();
                    mbResult = mbsArray[i].GetErrorStatus();
                    if(mbsRUNSTATE.DATAEXTRACTING == mbRunState || mbsRUNSTATE.FINISHED == mbRunState || mbsRESULT.OK != mbResult)
                    {
                        Console.WriteLine("Failed on instance " + i + ", call " + CallNum + ": " + mbsArray[i].MbsResultToString(mbResult));
                        return mbResult;
                    }
                } while(mbsRUNSTATE.RUNPAUSED != mbRunState);
            }


            return mbResult;
        }




        static int pinwheel = 0;
        static string DrawPinwheel()
        {
            string sz = "?";
            switch(pinwheel)
            {
                case 0:
                    sz = "|";
                    break;
                case 1:
                    sz =  "/";
                    break;
                case 2:
                    sz =  "-";
                    break;
                case 3:
                    sz =  "\\";
                    break;
            }
            pinwheel++;
            pinwheel %= 4;
            return sz;
        }
    }
}

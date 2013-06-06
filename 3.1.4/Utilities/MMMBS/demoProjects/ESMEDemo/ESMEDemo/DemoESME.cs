using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using mbs;
using System.Threading;
using System.IO;
using System.Diagnostics;

namespace ESMEDemo
{
    class DemoESME
    {
        static uint NUM_THROTTLE_ITERATIONS = 12; // the number of iteration 3MB is to run for each iteration/update of the calling application.
        static int NUMANIMATS = 50; // the number of animats to populate the scenario with.
        static int SCENARIO_DURATION_SECONDS = 5*60*60; // the duration in seconds to run the senario.
        static string BATHYMETRY_FILE = "bathymetry\\Bahamas.bth";
        static string SPECIES1_FILE = "species\\generic_mysticete.spe";
        static string SPECIES2_FILE = "species\\generic_odontocete.spe";
        static Boolean DURATIONLESS = true; // A feature for ESME so ESME doesn't have to tell 3MB ahead of time
                                            // the number of iterations to run.

        static C3mbs s_mb = new C3mbs();
        static mbsRESULT s_result;

        
#if false
        const string ROOTDIR = ".\\"; //"..\\..\\..\\..\\"; // root directory
        const string DEMODIR = ".\\";  //ROOTDIR + "demoFiles\\"; // directory holding demo files
        const string OUTPUTDIR = ".\\"; //ROOTDIR + "demoOutput\\"; // directory to store output (if any).
#else

        static string ROOTDIR = "..\\..\\..\\..\\..\\"; // root directory
        static string DEMODIR = ROOTDIR + "inputFiles\\"; // directory holding demo files
        static string OUTPUTDIR = ROOTDIR + "demoOutput\\"; // directory to store output (if any).
#endif


        static void Main(string[] args)
        {

            string sz = Directory.GetCurrentDirectory();
            DirectoryInfo di = Directory.GetParent(".");

            if(di.Parent.Name == "3mb")
            {
                ROOTDIR = ".\\";
                DEMODIR = ROOTDIR + "inputFiles\\";
                OUTPUTDIR = ROOTDIR + "demoOutput\\";
            }

            Console.WriteLine("Parent Directory: " + di.Parent.Name);
            Console.WriteLine("Current Directory:");
            Console.WriteLine(sz + "\n");
            // Set the output file (useful only if there is to be ouput)
            if(Directory.Exists(OUTPUTDIR) == false)
                Directory.CreateDirectory(OUTPUTDIR);
            if(mbsRESULT.OK != (s_result = s_mb.SetOutputDirectory(OUTPUTDIR)))
            {
                Console.WriteLine("\nFailed to set output directory");
                return;
            }

            // Demonstrate building a scenario, setting it to run durationless, and running it.
            Setup3MB();
            RunScenario();
        }


        //******************************************************************************//
        // Demonstrates how to build a scenario from scratch.
        //******************************************************************************//
        static void Setup3MB()
        {
            int i;
            int spe;
            mbsPosition pos;

            /*--------------------------------------------------------------------------//
             * Clear the information from the previous scenario
             *-----------------------------------------------------//
             *  If a previous scenario was set up the ClearScenario() function clears
             *  already present species models, bathymetry data, animats, and so on.
             *--------------------------------------------------------------------------*/
            s_mb.ClearScenario();

            /*--------------------------------------------------------------------------//
             * Load in Bathymetry data.
             *-------------------------//
             * Notes:
             *  (1) 3MB now requires its own copy of the bathymetry map in memory.  The
             *      The shore following animat behavior has the animats look ahead in time
             *      to determine if they will collide with the shore so it is no longer
             *      sufficient to pass to the animat the bathymetry value at its current
             *      location.
             *      
             *  (2) Bathymetry files with a ".bth" extension must be formatted with
             *      postive depth values and those with a .txt extension must be formatted
             *      with a negative depth values.  Both .bth and .txt files are text
             *      files.
             *--------------------------------------------------------------------------*/
            if(mbsRESULT.OK != (s_result = s_mb.LoadBathymetryFromTextFile(DEMODIR + BATHYMETRY_FILE)))
            {
                Console.WriteLine("\nBathymetry load Error");
                Process.GetCurrentProcess().Kill();
                return;
            }

            /*--------------------------------------------------------------------------//
             * Add two species
             *----------------*/
            if(mbsRESULT.OK != (s_result = s_mb.AddSpecies(DEMODIR + SPECIES1_FILE)))
            {
                Console.WriteLine("\nSpecies " + SPECIES1_FILE + " load Error");
                Process.GetCurrentProcess().Kill();
                return;
            }
            if(mbsRESULT.OK != (s_result = s_mb.AddSpecies(DEMODIR + SPECIES2_FILE)))
            {
                Console.WriteLine("\nSpecies " + SPECIES2_FILE + " load Error");
                Process.GetCurrentProcess().Kill();
                return;
            }


            /*--------------------------------------------------------------------------//
             * Populate each species with animats
             *-----------------------------------//
             * Notes
             *  (1) There are a few functions for populating animats into a species:
             *      (a) AddIndividualAnimat(int SpeciesIndex, mbsPosition Position)
             *      (b) AddIndividualAnimats(int SpeciesIndex, int NumAnimats, mbsPosition[] Position)
             *      (c) AddPod(int SpeciesIndex)
             *      (d) AddPod(int SpeciesIndex, mbsPODLEADERTYPE LeaderType, double FocalDistance, int NumAnimats, mbsPosition[] Position)
             *      (e) AddPodAnimat(int SpeciesIndex, int PodIndex, mbsPosition Position)
             *      (f) AddPodAnimats(int SpeciesIndex, int PodIndex, int NumAnimats, mbsPosition[] Position);
             *--------------------------------------------------------------------------*/
            pos = new mbsPosition();
            pos.latitude = .4;  // Faked initial lat
            pos.longitude = .4; // Faked initial lon
            for(spe=0; spe < s_mb.GetSpeciesCount(); spe++)
            {
                for(i=0; i<NUMANIMATS; i++)
                {
                    if(mbsRESULT.OK != (s_result = s_mb.AddIndividualAnimat(spe, pos)))
                    {
                        Console.WriteLine("Error populating species: " + s_mb.MbsResultToString(s_result));
                        Process.GetCurrentProcess().Kill();
                        return;
                    }
                }
            }

            // Feedback
            for(spe=0; spe<s_mb.GetSpeciesCount(); spe++)
            {
                Console.WriteLine("\nSpecies " + (spe+1) + " Animat Count: " +
                    s_mb.GetAnimatCount(spe));
                if(s_mb.GetAnimatCount(spe) < 1)
                {
                    Console.WriteLine("\nAn unpopulated species is present");
                    Process.GetCurrentProcess().Kill();
                    return;
                }
            }


            /*--------------------------------------------------------------------------//
             * Set 3MB to run for a specified duration or run durationless.
             *--------------------------------------------------------------//
             * Notes
             *  (1) See notes and call to SetDuration() about setting the duration.
             *  (2) Setting the duration isn't required if setting the scenario to run in
             *      durationless mode.
             *  (3) Setting 3MB to run duration less prevents it from outputting any data
             *      to file.
             *--------------------------------------------------------------------------*/
            mbsCONFIG c = s_mb.GetConfiguration();
            c.durationLess = DURATIONLESS;
            s_mb.SetConfiguration(c);


            /*--------------------------------------------------------------------------//
             * Set the duration of the scenario.
             *---------------------------------//
             * Notes
             * (1) Setting the duration isn't required if setting the scenario to run in
             *     durationless mode.
             * (2) ESME should set 3MB to run in durationless mode so that 3MB does not
             *     output any data to file and so ESME may run as long as it wishes
             *     without having to tell 3MB ahead of time how long to run.
             *
             *--------------------------------------------------------------------------*/
            if(c.durationLess == false)
                s_mb.SetDuration(SCENARIO_DURATION_SECONDS);
        }



        static void RunScenario()
        {
            int elapsedIterations = 0;
            int animatCnt = s_mb.GetAnimatCount();
            int speCnt = s_mb.GetSpeciesCount();
            int i;
            double acousticSrclat=0;
            double acousticSrclon=0;
            mbsRUNSTATE rs;
            mbsSCESTATE ss;
            mbsCONFIG config;
            uint numThrottleIterations = NUM_THROTTLE_ITERATIONS;
            int loop;

            // You don't want to set throttle durations to zero, especially if 3MB is run in
            // durationless mode.
            if(numThrottleIterations == 0)
                numThrottleIterations = 1;


            // Create the needed arrays (animat position plus any environmental data needing to be
            // set.  Only acoustic exposure and bathymetry are the only environmental data supported
            // right now, and bathymetry isn't functional yet.
            mbsPosition[] positionArray = new mbsPosition[animatCnt];
            double[] acousticExposureArray = new double[animatCnt];
            double[] bathymetryArray = new double[animatCnt];
            config = s_mb.GetConfiguration();


            /*--------------------------------------------------------------------------//
             * Print a nice startup banner
             *----------------------------*/
            PrintStartupBanner();

            /*--------------------------------------------------------------------------//
             * Make sure 3MB isn't doing anything.
             *------------------------------------//
             *  The run state is set to FINISHED only when
             *  (a) the 3MB libraries are instantiated
             *  (b) at the end of a scenario run.
             *  Verifying that the run state is in FINISHED here is just a check in case
             *  a scenario was previously run and shouldn't ever be the case unless a 
             *  mistake was made in the libary code somewhere or a unusual condition was
             *  encountered.
             *--------------------------------------------------------------------------*/
            while(mbsRUNSTATE.FINISHED != (rs = s_mb.GetRunState()))
            {
                Console.Write("Waiting on 3MB " + DrawPinwheel() + "      \r");
                Thread.Sleep(50);
            }

            /*--------------------------------------------------------------------------//
             * Initialize the scenario.
             *-------------------------//
             * Initialization sets up the scenario internally and determines the animats
             * initial state (location, dive activity, depth and so on).
             *--------------------------------------------------------------------------*/
            if(mbsRESULT.OK != (s_result = s_mb.InitializeRun()))
            {
                Console.WriteLine("\nCritical Error Initializing 3mbs: " + s_mb.MbsResultToString(s_result));
                Process.GetCurrentProcess().Kill();
                return;
            }
            while((rs = s_mb.GetRunState()) == mbsRUNSTATE.INITIALIZING)
            {
                Console.Write("Initializing 3MB " + DrawPinwheel() + "    \r");
                Thread.Sleep(50);
            }


            /*--------------------------------------------------------------------------//
            // Throttle the 3MB
             *-----------------//
             * At this point the animats are still in their initial state with a possble
             * initial acoustic exposre value set.  The following loop shows how to
             * advance the 3MB a specific number of iterations then pause so the animat
             * locations can be again accessed, their acoustic again set, and so that the
             * calling application can update itself.
             *--------------------------------------------------------------------------*/
            Console.Write("3MB Running                                \r");
            while((rs = s_mb.GetRunState()) != mbsRUNSTATE.FINISHED)  // until 3MB is finished...
            {
                if((rs = s_mb.GetRunState()) == mbsRUNSTATE.RUNPAUSED) // if 3MB is ready to be throttled...
                {

                    /*----------------------------------------------------------------------//
                     * Get the animats' current location and set acoustic exposure
                     *------------------------------------------------------------//
                     * Notes
                     *  (1) Once the scenario is running the ONLY VALID opportunity to set the
                     *      animats' acoustic state values is when 3MB is paused as it waits
                     *      to be throttled.  Retrieving the animat's locations, however, is
                     *      permited even if the 3MB isn't paused.
                     *----------------------------------------------------------------------*/
                    if(mbsRESULT.OK != (s_result = s_mb.GetAnimatCoordinates(positionArray)))
                    {
                        s_mb.AbortRun(); // kills the thread.
                        Console.WriteLine("\nCritical Error Fetching Animat Coordinates: " + s_mb.MbsResultToString(s_result));
                        Process.GetCurrentProcess().Kill();
                        return;
                    }


                    // Acoustic source location is faked for this demo.
                    acousticSrclat += 0.00001;
                    acousticSrclon -= 0.00001;

                    for(i=0; i<animatCnt; i++)
                        acousticExposureArray[i] = i; // fake the acoustic exposure data again for this demo

                    // Set the acoustic source location and animat aoustic exposure.
                    try
                    {
                        if(mbsRESULT.OK != (s_result = s_mb.SetAnimatAcousticExposure(acousticSrclat, acousticSrclon, acousticExposureArray)))
                        {
                            s_mb.AbortRun(); // kills the thread.
                            Console.WriteLine("\nCritical Error Setting Acoustic Exposure: " + s_mb.MbsResultToString(s_result));
                            Process.GetCurrentProcess().Kill();
                            return;
                        }
                    }
                    catch
                    {
                        s_mb.AbortRun(); // kills the thread.
                        Console.WriteLine("\nCritical Error Setting Acoustic Exposure: " + s_mb.MbsResultToString(s_result));
                        Process.GetCurrentProcess().Kill();
                        return;
                    }


                    /*----------------------------------------------------------------------//
                     * Throttle the 3MB.
                     *-----------------//
                     * (1) The calling application sets the number if iterations 3MB iterates
                     *     and is permitted to vary.
                     * (2) The StepRun() function will be only briefily blocked pending the
                     *     release of an mutex then returns while the 3MB iterates for the
                     *     number of iterations it was set to.
                     * (3) Entering a 0 for the number of iterations to throttle is legal but
                     *     nothing will happen.
                     *----------------------------------------------------------------------*/
                    Debug.Assert(numThrottleIterations != 0); // Pointless and causes needless looping.
                    if(mbsRESULT.OK != (s_result = s_mb.StepRun(numThrottleIterations)))
                    {
                        s_mb.AbortRun();
                        Console.WriteLine("\nCritical Error running 3mbs: " + s_mb.MbsResultToString(s_result) + " at " +
                            elapsedIterations + " of " + s_mb.GetDurationSeconds());
                        Process.GetCurrentProcess().Kill();
                        return;
                    }
                }


                /*----------------------------------------------------------------------//
                 * Setting 3MB to throttle was successful.  Now do stuff until 3MB
                 * completes this round of throttled iterations.
                 *----------------------------------------------------------------------//
                 * (1) 3MB's run state is set to RUNNING while it is iterating the number
                 *     of iterations it was set to throtle..
                 *----------------------------------------------------------------------*/
                loop = 0;
                while((rs = s_mb.GetRunState()) == mbsRUNSTATE.RUNNING)
                {
                    Thread.Sleep(1);
                    ss = s_mb.GetScenarioState();
                    elapsedIterations = (int)ss.currentIteration;
                    Console.Write("3MB iterating in background, loop(" + loop++ + ") " + elapsedIterations + " of " + SCENARIO_DURATION_SECONDS + "              \r");
                }


                /*----------------------------------------------------------------------//
                 * Track the number of iterations elapsed and handle it based on if the
                 * calling application set 3MB to run durationless or duration-limited.
                 *---------------------------------------------------------------------//
                 * Notes
                 *  (1) ESME is expected to run in a durationless mode where it simply
                 *      keeps 3MB running as long as the ESME application is running.
                 *  (2) If 3MB configured to run in durationless mode it must tell 3MB
                 *      when to shut down.  The conditional below shows how that is done.
                 *----------------------------------------------------------------------*/
                if(config.durationLess == true)
                {
                    elapsedIterations += (int)numThrottleIterations;
                    if(elapsedIterations >= SCENARIO_DURATION_SECONDS)
                    {
                        Console.WriteLine("\nInstructing 3MB to cease running\n");
                        s_mb.AbortRun(); // Tell 3MB to shut down.
                        while((rs = s_mb.GetRunState()) != mbsRUNSTATE.FINISHED)
                        {
                            Console.Write("Waiting on 3MB to shut down " + DrawPinwheel() + "           \r");
                            Thread.Sleep(5);
                        }
                    }
                }
                else
                {
                    ss = s_mb.GetScenarioState();
                    elapsedIterations = (int)ss.currentIteration;
                }
            }
            Console.WriteLine("\nDone\n");
        }


        static void PrintStartupBanner()
        {
            mbsBUILDINF buildInf = s_mb.GetBuildInformation();
            mbsCONFIG config = s_mb.GetConfiguration();

            Console.WriteLine("********************************************************");
            Console.Write("\n\n3MB Version " + s_mb.LibraryVersionSuper() + "."
                + s_mb.LibraryVersionSub());
            if(buildInf.buildType == mbsBUILDTYPE.MBDEBUG)
                Console.Write(" (Debug) ");
            else
                Console.Write(" (Release) ");
            Console.Write("" + buildInf.bitSize + " bit ");
            Console.Write(buildInf.szBuildDate + " ");
            Console.WriteLine(buildInf.szBuildTime);

            if(config.durationLess == true)
                Console.WriteLine("durationless");
            else
                Console.WriteLine("duration-limited");
            Console.WriteLine("" + s_mb.GetSpeciesCount() + " species");
            Console.WriteLine("" + s_mb.GetAnimatCount() + " animats");
            Console.WriteLine("" + s_mb.GetDurationSeconds() + " seconds");
            Console.WriteLine("" + s_mb.GetBathymetryFileName());
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

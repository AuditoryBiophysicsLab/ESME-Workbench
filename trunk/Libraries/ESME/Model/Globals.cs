using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;
using System.IO;

namespace ESME.Model
{
    public static class Globals
    {
        private const string TransmissionLossFolderName = "Transmission Loss";
        /// <summary>
        /// Get a new, random name for a runfile in the current soundfield folder
        /// </summary>
        public static string RandomRunFileFilename
        {
            get
            {
                string RandomName = Path.GetFileNameWithoutExtension(Path.GetRandomFileName()) + ".run";
                return Path.Combine(CurrentTransmissionLossFolder, RandomName);
            }
        }
        /// <summary>
        /// The path of the currently active Transmission Loss folder
        /// </summary>
        public static string CurrentTransmissionLossFolder
        {
            get
            {
                return Path.Combine(CurrentExperimentFolder, TransmissionLossFolderName);
            }
        }

        public static string CurrentEnvironmentFile
        {
            get
            {
                if ((CurrentLocationName == null) || (CurrentLocationName == string.Empty))
                    throw new FileNotFoundException("CurrentEnvironmentFile is undefined because CurrentLocationName is not set");
                return Path.Combine(UserLocationsFolder, CurrentLocationName + ".eeb");
            }
        }

        public static string CurrentExperimentFolder
        {
            get { return Path.Combine(UserExperimentsFolder, CurrentExperimentName); }
        }

        public static string[] AllLocationFiles
        {
            get
            {
                return Directory.GetFiles(UserLocationsFolder, "*.eeb");
            }
        }

        public static string[] AllLocationNames
        {
            get
            {
                List<string> Locations = new List<string>();
                foreach (string l in AllLocationFiles)
                    Locations.Add(Path.GetFileNameWithoutExtension(l));
                return Locations.ToArray();
            }
        }

        public static string[] AllExperimentFolders
        {
            get
            {
                return Directory.GetDirectories(UserExperimentsFolder);
            }
        }

        public static string[] AllExperimentNames
        {
            get
            {
                List<string> Experiments = new List<string>();
                foreach (string e in AllExperimentFolders)
                    Experiments.Add(Path.GetFileName(e));
                return Experiments.ToArray();
            }
        }

        public static string FindExperimentFile(string ExperimentName)
        {
            return Path.Combine(Path.Combine(UserExperimentsFolder, ExperimentName), ExperimentName + ".esme");
        }

        public static IEnumerable<string> AllTransmissionLossFolders
        {
            get
            {
                foreach (string l in AllExperimentFolders)
                {
                    string target = Path.Combine(l, TransmissionLossFolderName);
                    if (Directory.Exists(target))
                        yield return target;
                }
            }
        }

        /// <summary>
        /// The name of the current location, e.g. Bahamas
        /// </summary>
        public static string CurrentLocationName 
        {
            get { return _CurrentLocationName; }
            set
            {
                _CurrentLocationName = value;
                if (!Directory.Exists(UserLocationsFolder))
                    Directory.CreateDirectory(UserLocationsFolder);
            }
        }

        public static string CurrentExperimentName
        {
            get { return _CurrentExperimentName; }
            set
            {
                _CurrentExperimentName = value;
                if (!Directory.Exists(CurrentTransmissionLossFolder))
                    Directory.CreateDirectory(CurrentTransmissionLossFolder);
            }
        }

        /// <summary>
        /// The location of the user's ESME WorkBench folder, typically in \Users\[Username]\Documents\ESME WorkBench
        /// </summary>
        public static string UserESMEFolder { get; internal set; }

        /// <summary>
        /// The location of the user's Locations folder, typically in \Users\[Username]\Documents\ESME WorkBench\Locations
        /// </summary>
        public static string UserLocationsFolder { get; internal set; }

        /// <summary>
        /// The location of the users's Experiments folder, typically in \Users\[Username]\Documents\ESME WorkBench\Experiments
        /// </summary>
        public static string UserExperimentsFolder { get; internal set; }

        /// <summary>
        /// The location of the system's Locations folder, typically in Program Files\ESME WorkBench\Locations
        /// </summary>
        public static string SystemLocationsFolder { get; internal set; }

        /// <summary>
        /// The location of the system's Species folder, typically in Program Files\ESME WorkBench\Animats
        /// </summary>
        public static string SystemSpeciesFolder { get; internal set; }

        /// <summary>
        /// The location of the system's Sample Experiments folder, typically in Program Files\ESME WorkBench\Sample Experiments
        /// </summary>
        public static string SystemSamplesFolder { get; internal set; }

        /// <summary>
        /// The location of the system's Documents folder, typically in Program Files\ESME WorkBench\Documentation
        /// </summary>
        public static string SystemDocumentsFolder { get; internal set; }

        /// <summary>
        /// The location of the system's Tools folder, typically in Program Files\ESME WorkBench\Tools
        /// </summary>
        public static string SystemToolsFolder { get; internal set; }

        /// <summary>
        /// The location of the application installation folder, typically in Program Files\ESME WorkBench
        /// </summary>
        public static string ApplicationRootFolder { get; internal set; }

        static Globals()
        {
            Globals.ApplicationRootFolder = Path.GetDirectoryName(Assembly.GetCallingAssembly().Location);
            Globals.SystemToolsFolder = Path.Combine(Globals.ApplicationRootFolder, "Tools");
            Globals.SystemDocumentsFolder = Path.Combine(Globals.ApplicationRootFolder, "Documentation");
            Globals.SystemSamplesFolder = Path.Combine(Globals.ApplicationRootFolder, "Sample Experiments");
            Globals.SystemSpeciesFolder = Path.Combine(Globals.ApplicationRootFolder, "Animats");
            Globals.SystemLocationsFolder = Path.Combine(Globals.ApplicationRootFolder, "Locations");

            Globals.UserESMEFolder = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "ESME WorkBench");
            Globals.UserLocationsFolder = Path.Combine(Globals.UserESMEFolder, "Locations");
            Globals.UserExperimentsFolder = Path.Combine(Globals.UserESMEFolder, "Experiments");
            if (!Directory.Exists(UserESMEFolder))
                Directory.CreateDirectory(UserESMEFolder);
            if (!Directory.Exists(UserLocationsFolder))
                Directory.CreateDirectory(UserLocationsFolder);
            if (!Directory.Exists(UserExperimentsFolder))
                Directory.CreateDirectory(UserExperimentsFolder);
        }

        private static string _CurrentLocationName, _CurrentExperimentName;

    }
}

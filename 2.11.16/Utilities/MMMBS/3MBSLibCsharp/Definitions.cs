using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;

namespace MMMBSLib
{
    public struct DISTANGL
    {
        public double distance;
        public double angle;
    }

    public struct WIDTHHEIGHT
    {
        public int w;
        public int h;
    }

    public struct INTSPAN
    {
        public int start;
        public int end;
    }

/*
    public struct DEPTHTIME
    {
        public double depth;
        public double time;
    };
    */
    public struct HHMMSS { public uint hh; public uint mm; public uint ss;};
    public struct XYCOORD { public int x; public int y;}
    public struct INTMINMAX { public int min; public int max;}
    public struct DATASTRINGTITLE { public string titleString; public string dataString;}
    public struct MATRIXDIMENSIONS { public int maxCols; public int minCols; public int rowCnt; public int[] colCnts;}
    public struct VECTORDIMENSIONS
    {
        public int rowCnt;
        public int colCnt;
    }
    public class STRINGCONSTANTS
    {
        public static string NOBEHAVIORINFLUENCE = "No influence defined for this behavior";
        public static string SZ_NOTAPPLICABLE= "Not Applicable";

        public static string SZ_UNSET = "Unset";
        public static string SZ_TIME = "Time";
        public static string SZ_TEMPERATURE = "Temperature";
        public static string SZ_DEPTH = "Depth";
        public static string SZ_TIME_TEMPERATURE = "Time, Temperature";
        public static string SZ_TIME_DEPTH = "Time, Depth";
        public static string SZ_TIME_TEMPERATURE_DEPTH = "Time, Temperature, Depth";

        public static string SZ_LEADANIMAT = "Focal Animat";
        public static string SZ_CALCULATEDCENTROID = "Calculated Centroid";
        public static string SZ_OVERRIDE_ACOUSTICEXPOSURE = "Acoustic Exposure";

        public static string SZ_DISABLED = "Disabled";
        public static string SZ_BOOLEAN = "Boolean";
        public static string SZ_GAUSSIAN = "Gaussian";
        public static string SZ_RANDOM = "Random";
        public static string SZ_EXTRNLLYMDLED = "Externally Modeled";
        public static string SZ_MATRIX = "Vector";
        public static string SZ_DIRECTIONMATRIXWITHBIAS = "Vector with Bias";
        public static string SZ_DIRECTIONMATRIXNOBIAS = "Vector without Bias";
        public static string SZ_RANDOMWALK = "Random Walk";
        public static string SZ_CORR_RAND_WALK   = "Correlated Random Walk";
        public static string SZ_CORR_RAND_WALKDB = "Correlated Random Walk DB";
        public static string SZ_STIMULIVECTOR = "Vector";
        public static string SZ_STIMULIVALUE = "Value";
    }

    public enum ENVATTRACTORPRIORITY { DIVE, TEMPERATURE, }
    public enum PODTYPE { LEAD_ANIMAT, CALCULATED_CENTROID, }
    public enum BEHAVIORMODELTYPE { DISABLED, VECTOR, }
    public enum FLATBOTTOMDLTYPE { NO_FLTBTTMDIVING, BOTTOMFOLLOWS_CURRENT_VELOCITY, BOTTOMFOLLOWS_GAUSSIAN_VELOCITY, BOTTOMFOLLOWS_UNIFORM_VELOCITY}
    public enum DIRECTIONMODELTYPE { RANDOM_WALK, VECTOR, VECTOR_DIRBIAS, CORR_RAND_WALK, CORR_RAND_WALK_DIR_BIAS}
    public enum MODELTYPE { GAUSSIAN, RANDOM, MATRIX, DISABLED, EXTERNALLYMODELED,}
    public enum REVERSAL_DIVE_RATE_TYPE { NO_INDEPENDENT, INDEPENDENT, INDEPENDENT_DIVE_AND_ASCENT, }
    public enum SRFINVMODELTYPE { GAUSSIAN, DONNOTUSERANDOM, MATRIX, }
    public enum STIMULIMODELTYPE { VALUE, VECTOR, }
    public enum TRANSITIONALMODELTYPE { BEHAVIORAL, DEPTHENV, TEMPERATUREENV}
    public enum BEHTRANS_TERM_MODEL { T50_K_TERM, GAUSSIAN_TERM }

    // Acoustic Exposure Enumerations
    public enum SOUNDPRESSUREUNIT { MICRO_PA, MICRO_PA2S}
    public enum DECAYFUNCTIONS { DISABLED, DECAYFNC1, DECAYFNC2, DECAYFNC3, }

    // Added Acoustic Exposure stuff, need to be added to data structures and wrapper class passing
    public enum DOSERESPONSE { ODONTOCETE, MYSTICETE, OTARIID, PHOCID}
    public enum AETYPE { DOSE, THRESHOLD }
    // Need to add a boolean to acoustic threshold (and possibly then deactivation threshold) into the
    // wrapper classes



    public enum RESLT
    {
        // Non-errors.
        OK,
        OK_EOFREACHED,
        OK_FILESNOTMATCH,
        CANCEL,

        // Errors.
        ALREADYRUNNING_ERROR,
        MEMALLOC_ERROR,
        FILEFORMAT_ERROR, 	// Error with the file... as in not formatted properly.
        FILENAME_ERROR,
        OPENFILEREAD_ERROR,
        OPENFILEWRITE_ERROR,
        OPENTEXTOUTPUTFILE_ERROR,
        CREATEBINARYOUTPUT_ERROR,

        SETFILEPOINTER_ERROR,

        FILEREAD_ERROR,	// can be either file read error or buffer read error.
        FILEWRITE_ERROR,
        WRONGFILETYPE_ERROR,
        INVALIDHANDLE_ERROR,
        USERMODELLINELENGTHEXCEEDED_ERROR,
        UNRECOGNIZED_SPECIES_MATRIX_PARAM_ERROR,

        // Setup Errors
        NOSPECIESLOADED_ERROR,
        NOANIMATPOPULATION_ERROR,
        UNPOPULATEDSPECIES_ERROR,
        POPLIMITEXCEEDED_ERROR,
        MBS_SETUP_ERROR_
    };
}

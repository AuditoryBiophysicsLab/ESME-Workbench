using FileHelpers;

namespace ImportPSM
{
    /// <summary>
    /// A Record class for Filehelpers CSV parsing.  Each public property corresponds, very possibly in order, to each field in a csv file's line. 
    /// We skip the first three lines of the file for PSM.CSV files corresponding to NUWC's NEMO2 system that is current as of 20 NOV 2011. 
    /// Not guaranteed to work with very early (pre-2011) PSM files (which may be missing the first line denoting CLASS/UNCLASS) or post JAN2012 files (which postdate the latest version available at this writing)
    /// 
    /// For valid files, line 0 is a !-flagged classification line, lines 1 and 2 are #-commented field name and corresponding unit identifiers.  Lines 3-n are record entries.
    /// </summary>
    [DelimitedRecord(",")]
    [IgnoreFirst(3)]
    public class PSMFileRecord
    {
        public string PlatformType;
        public string PlatformName;
        public string SourceType;
        public string SourceName;
        public string ModeType;
        public string ModeName;
        public float? ActiveTime;
        public float? Depth;
        public float SourceLevel;
        public float LowFrequency;
        public float HighFrequency;
        public float PulseInterval;
        public float PulseLength;
        public float HorizontalBeamwidth;
        public float VerticalBeamwidth;
        public float DepressionElevationAngle;
        public float RelativeBeamAngle;
        public float MaxPropagationRadius;
    }
}
using System;
using System.IO;
using System.Linq;
using FileHelpers;

namespace ESME.Database.Importers
{
    public static class PSMFile
    {
        public static void Import(string csvFilePath, PSMContext psm)
        {
            var engine = new FileHelperEngine(typeof (PSMFileRecord));
            var entries = (PSMFileRecord[])engine.ReadFile(csvFilePath);

            foreach (var entry in entries)
            {
                var platform = (from p in psm.Platforms
                                where p.PlatformName == entry.PlatformName
                                select p).FirstOrDefault();
                if (platform == null)
                {
                    platform = new Platform
                    {
                        PlatformName = entry.PlatformName,
                        PlatformType = entry.PlatformType,
                    };
                    psm.Platforms.Add(platform);
                    psm.SaveChanges();
                }
                Source source = null;
                if (platform.Sources != null)
                {
                    source = (from s in platform.Sources
                              where s.SourceName == entry.SourceName
                              select s).FirstOrDefault();

                }
                if (source == null)
                {
                    source = new Source
                    {
                        SourceName = entry.SourceName,
                        SourceType = entry.SourceType,

                        Platform = platform,
                    };
                    psm.Sources.Add(source);
                    psm.SaveChanges();
                }

                Mode mode = null;
                if (source.Modes != null)
                {
                    mode = (from m in source.Modes
                            where m.ModeName == entry.ModeName && m.ModeType == entry.ModeType
                            select m).FirstOrDefault();
                }
                if (mode == null)
                {
                    mode = new Mode
                    {
                        Source = source,
                        ActiveTime = entry.ActiveTime,
                        Depth = entry.Depth,
                        SourceLevel = entry.SourceLevel,
                        LowFrequency = entry.LowFrequency,
                        HighFrequency = entry.HighFrequency,
                        PulseInterval = entry.PulseInterval,
                        PulseLength = entry.PulseLength,
                        HorizontalBeamWidth = entry.HorizontalBeamwidth,
                        VerticalBeamWidth = entry.VerticalBeamwidth,
                        DepressionElevationAngle = entry.DepressionElevationAngle,
                        RelativeBeamAngle = entry.RelativeBeamAngle,
                        MaxPropagationRadius = entry.MaxPropagationRadius,
                        ModeName = entry.ModeName,
                        ModeType = entry.ModeType,
                    };
                    psm.Modes.Add(mode);
                    psm.SaveChanges();
                }
            }
        }

        public static void Export(string csvFilePath, PSMContext psm)
        {
            using (var writer = new StreamWriter(csvFilePath))
            {
                writer.WriteLine(@"!UNCLASSIFIED,3,3,,,,,,,,,,,,,,,");
                writer.WriteLine(@"#Platform Type,Platform Name,Source Type,Source Name,Mode Type,Mode Name,Active Time,Depth,Source Level,Low Frequency,High Frequency,Pulse Interval,Pulse Length,Horizontal Beamwidth,Vertical BeamWidth,DE Angle,Relative Beam Angle,Max Propagation Radius");
                writer.WriteLine(@"#Units,,,,,,seconds,meters,dB,Hz,Hz,seconds,milliseconds,degrees,degrees,degrees,degrees,meters");
                foreach (var platform in psm.Platforms)
                    foreach (var source in platform.Sources)
                        foreach (var mode in source.Modes)
                            writer.WriteLine("{0},{1},{2},{3},{4},{5},{6},{7},{8},{9},{10},{11},{12},{13},{14},{15},{16},{17}",
                                             platform.PlatformType,
                                             platform.PlatformName,
                                             source.SourceType,
                                             source.SourceName,
                                             mode.ModeType,
                                             mode.ModeName,
                                             mode.ActiveTime,
                                             mode.Depth,
                                             mode.SourceLevel,
                                             mode.LowFrequency,
                                             mode.HighFrequency,
                                             mode.PulseInterval,
                                             mode.PulseLength,
                                             mode.HorizontalBeamWidth,
                                             mode.VerticalBeamWidth,
                                             mode.DepressionElevationAngle,
                                             mode.RelativeBeamAngle,
                                             mode.MaxPropagationRadius);
            }
        }
    }
}

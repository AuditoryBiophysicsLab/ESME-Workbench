﻿using System;
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
                var platform = (from p in psm.PSMPlatforms
                                where p.PlatformName == entry.PlatformName
                                select p).FirstOrDefault();
                if (platform == null)
                {
                    platform = new PSMPlatform
                    {
                        PlatformName = entry.PlatformName,
                        PlatformType = entry.PlatformType,
                    };
                    psm.PSMPlatforms.Add(platform);
                    psm.SaveChanges();
                }
                PSMSource source = null;
                if (platform.PSMSources != null)
                {
                    source = (from s in platform.PSMSources
                              where s.SourceName == entry.SourceName
                              select s).FirstOrDefault();

                }
                if (source == null)
                {
                    source = new PSMSource
                    {
                        SourceName = entry.SourceName,
                        SourceType = entry.SourceType,

                        PSMPlatform = platform,
                    };
                    psm.PSMSources.Add(source);
                    psm.SaveChanges();
                }

                PSMMode mode = null;
                if (source.PSMModes != null)
                {
                    mode = (from m in source.PSMModes
                            where m.ModeName == entry.ModeName && m.ModeType == entry.ModeType
                            select m).FirstOrDefault();
                }
                if (mode != null) continue;
                mode = new PSMMode
                {
                    Source = source,
                    ActiveTime = entry.ActiveTime,
                    Depth = entry.Depth,
                    SourceLevel = entry.SourceLevel,
                    LowFrequency = entry.LowFrequency,
                    HighFrequency = entry.HighFrequency,
                    PulseInterval = new TimeSpan((int)(entry.PulseInterval * 10000000)),    // Ticks are 100ns, this converts the seconds value in the PSM file to ticks
                    PulseLength = new TimeSpan((int)(entry.PulseLength * 10000)),           // Ticks are 100ns, this converts the millisecond value in the PSM file to ticks
                    HorizontalBeamWidth = entry.HorizontalBeamwidth,
                    VerticalBeamWidth = entry.VerticalBeamwidth,
                    DepressionElevationAngle = entry.DepressionElevationAngle,
                    RelativeBeamAngle = entry.RelativeBeamAngle,
                    MaxPropagationRadius = entry.MaxPropagationRadius,
                    ModeName = entry.ModeName,
                    ModeType = entry.ModeType,
                };
                psm.PSMModes.Add(mode);
                psm.SaveChanges();
            }
        }

        public static void Export(string csvFilePath, PSMContext psm)
        {
            using (var writer = new StreamWriter(csvFilePath))
            {
                writer.WriteLine(@"!UNCLASSIFIED,3,3,,,,,,,,,,,,,,,");
                writer.WriteLine(@"#Platform Type,Platform Name,Source Type,Source Name,Mode Type,Mode Name,Active Time,Depth,Source Level,Low Frequency,High Frequency,Pulse Interval,Pulse Length,Horizontal Beamwidth,Vertical BeamWidth,DE Angle,Relative Beam Angle,Max Propagation Radius");
                writer.WriteLine(@"#Units,,,,,,seconds,meters,dB,Hz,Hz,seconds,milliseconds,degrees,degrees,degrees,degrees,meters");
                foreach (var platform in psm.PSMPlatforms)
                    foreach (var source in platform.PSMSources)
                        foreach (var mode in source.PSMModes)
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
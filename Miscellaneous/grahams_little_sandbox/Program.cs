using System;
using System.Collections.ObjectModel;
using System.Linq;
using ESME.PSM;

namespace grahams_little_sandbox
{
    class Program
    {
        static void Main()
        {
            var mode = new Mode()
            {
                Name = "test mode",
                Type = ModeType.NotSpecified,
                MaxPropagationRadius = 20000,
                DepthOffset = 1,
                DepressionElevationAngle = 90,
                Frequency = 1000,
                HorizontalBeamWidth = 180,
                PulseInterval = TimeSpan.FromSeconds(30),
                PulseLength = TimeSpan.FromMilliseconds(100),
                RelativeBeamAngle = 0,
                VerticalBeamWidth = 90,
                SideLobeAttenuation = 40,
                SourceLevel = 200,
            };

            var source = new Source
            {
                Name = "test source",
                Modes = new Collection<Mode>(),
                Type = SourceType.NotSpecified,
            };
            var platform = new Platform()
            {
                Name = "test platform",
                IsRandom = false,
                Speed = 4,
                Type = PlatformType.NotSpecified,
                Sources = new Collection<Source>(),
            };

            platform.Sources.Add(source);
            source.Modes.Add(mode);
            source.Platform = platform;
            mode.Source = source;


            PSMContext.Modify(p => p.Platforms.Add(platform));
            var result = PSMContext.Query(p => p.Platforms.ToList());
        }
    }
}

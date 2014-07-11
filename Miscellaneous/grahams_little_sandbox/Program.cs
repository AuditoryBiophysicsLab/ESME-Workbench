using System.Collections.ObjectModel;
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
            };

            var source = new Source
            {
                Name = "test source",
                Modes = new Collection<Mode>(),
            };
            var platform = new Platform()
            {
                Description = "test platform",
                IsRandom = false,
                Speed = 4,
                Type = PlatformType.NotSpecified,
                Sources = new Collection<Source>(),
            };

       

            PSMContext.Modify(p =>
            {
                platform.Sources.Add(source);
                source.Modes.Add(mode);
                source.Platform = platform;
                mode.Source = source;
                p.Platforms.Add(platform);
            });
        }
    }
}

using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Threading;
using ESME;
using ESME.Environment;
using ESME.Model;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using HRC;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using HRC.ViewModels;
using StandardTransmissionLossEngines.Controls;

namespace StandardTransmissionLossEngines
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ESMEPlugin(Name = "Bellhop",
                Description = "Bellhop is a highly efficient ray tracing program, written by Michael Porter of hlsresearch.com as part of the Acoustic Toolbox.")]
    public class BellhopEngine : TransmissionLossCalculatorPluginBase
    {
        [UsedImplicitly] PropertyObserver<BellhopEngine> _propertyObserver;
        public BellhopEngine() 
        {
            PluginSubtype = PluginSubtype.Bellhop;
            ConfigurationControl = new BellhopConfigurationControl { DataContext = this };
            Initialize();
            _propertyObserver = new PropertyObserver<BellhopEngine>(this)
                .RegisterHandler(p => p.RangeCellSize, Save)
                .RegisterHandler(p => p.DepthCellSize, Save)
                .RegisterHandler(p => p.UseSurfaceReflection, Save)
                .RegisterHandler(p => p.GenerateArrivalsFile, Save)
                .RegisterHandler(p => p.RayCount, Save);
        }

        void Initialize()
        {
            SetPropertiesFromAttributes(GetType());
            if (!File.Exists(ConfigurationFile)) Save();
            IsConfigured = true;
        }

        protected override void Save()
        {
            var serializer = new XmlSerializer<BellhopEngine> { Data = this };
            serializer.Save(ConfigurationFile, null);
        }

        public override void LoadSettings()
        {
            var settings = XmlSerializer<BellhopEngine>.LoadExistingFile(ConfigurationFile, null);
            if (settings == null) return;
            RangeCellSize = settings.RangeCellSize;
            DepthCellSize = settings.DepthCellSize;
            UseSurfaceReflection = settings.UseSurfaceReflection;
            GenerateArrivalsFile = settings.GenerateArrivalsFile;
            RayCount = settings.RayCount;
        }

        [Initialize(10.0)]  public double RangeCellSize { get; set; }
        [Initialize(10.0)]  public double DepthCellSize { get; set; }
        [Initialize(true)]  public bool UseSurfaceReflection { get; set; }
        [Initialize(false)] public bool GenerateArrivalsFile { get; set; }
        [Initialize(3000)]  public int RayCount { get; set; }

        public override void CalculateTransmissionLoss(Platform platform, Mode mode, Radial radial, BottomProfile bottomProfile, SedimentType sedimentType, double windSpeed, IList<Tuple<double, SoundSpeedProfile>> soundSpeedProfilesAlongRadial)
        {
            var depthCellCount = (int)Math.Ceiling(bottomProfile.MaxDepth / DepthCellSize);
            var rangeCellCount = (int)Math.Ceiling(mode.MaxPropagationRadius / RangeCellSize);
            var startProfile = soundSpeedProfilesAlongRadial[0].Item2;
            var sourceDepth = platform.Depth;
            var frequency = (float)Math.Sqrt(mode.HighFrequency * mode.LowFrequency);
            if (mode.Depth.HasValue) sourceDepth += mode.Depth.Value;
            var maxCalculationDepthMeters = bottomProfile.MaxDepth * 1.01;
            var directoryPath = Path.GetDirectoryName(radial.BasePath);
            if (directoryPath == null) throw new NullReferenceException("radial.BasePath does not point to a valid directory");
            if (!Directory.Exists(directoryPath)) Directory.CreateDirectory(directoryPath);

            using (var envFile = new StreamWriter(radial.BasePath + ".env", false))
            {
                envFile.WriteLine("'Bellhop'");
                envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "{0}", frequency));
                envFile.WriteLine("1"); // was NMEDIA in gui_genbellhopenv.m
                envFile.WriteLine(UseSurfaceReflection ? "'QFLT'" : "'QVLT'");

                //if (depthCellCount < 5) throw new BathymetryTooShallowException("Error: Maximum depth of transect (" + maxCalculationDepthMeters + " meters) less than minimum required for transmission loss calculations.\nPlease choose a different location for this transect.");

                envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "0, 0.0, {0}", startProfile.Data[startProfile.Data.Count - 1].Depth));
                foreach (var soundSpeedSample in startProfile.Data)
                    envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "{0} {1} 0.0 1.0 0.0 0.0", soundSpeedSample.Depth, soundSpeedSample.SoundSpeed));

                envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "'A*' 0.0")); // A = Acoustic halfspace, * = read bathymetry file 'BTYFIL', 0.0 = bottom roughness (currently ignored)
                envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "{0} {1} {2} {3} {4} {5} /", maxCalculationDepthMeters, sedimentType.CompressionWaveSpeed, sedimentType.ShearWaveSpeed, sedimentType.Density, sedimentType.LossParameter, 0));
                // Source and Receiver Depths and Ranges
                envFile.WriteLine("1"); // Number of Source Depths
                envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "{0} /", sourceDepth)); // source depth
                envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "{0}", depthCellCount)); // Number of Receiver Depths
                envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "0.0 {0} /", maxCalculationDepthMeters));
                envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "{0}", rangeCellCount)); // Number of receiver ranges
                envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "0.0 {0} /", mode.MaxPropagationRadius / 1000.0));

                envFile.WriteLine(GenerateArrivalsFile ? "'AB'" : "'I'");
                envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "{0}", RayCount)); // Number of beams
                var verticalHalfAngle = mode.VerticalBeamWidth / 2;
                var angle1 = mode.DepressionElevationAngle - verticalHalfAngle;
                var angle2 = mode.DepressionElevationAngle + verticalHalfAngle;
                envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "{0} {1} /", angle1, angle2)); // Beam fan half-angles (negative angles are toward the surface
                envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "0.0 {0} {1}", maxCalculationDepthMeters, (mode.MaxPropagationRadius / 1000.0) * 1.01)); // step zbox(meters) rbox(km)
            }
            using (var sspFile = new StreamWriter(radial.BasePath + ".ssp", false))
            {
                if (soundSpeedProfilesAlongRadial.Count == 1) soundSpeedProfilesAlongRadial.Add(Tuple.Create(Geo.RadiansToKilometers(radial.Segment.LengthRadians), new SoundSpeedProfile(soundSpeedProfilesAlongRadial[0].Item2)));
                sspFile.WriteLine("{0}", soundSpeedProfilesAlongRadial.Count);
                foreach (var rangeProfileTuple in soundSpeedProfilesAlongRadial) sspFile.Write("{0,-10:0.###}", rangeProfileTuple.Item1);
                sspFile.WriteLine();
                //sspFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "{0,-10:0.###}{1,-10:0.###}{2,-10:0.###}", 0.0, bottomProfile.Profile[bottomProfile.Profile.Count / 2].Range, bottomProfile.Profile[bottomProfile.Profile.Count - 1].Range));
                for (var depthIndex = 0; depthIndex < startProfile.Data.Count; depthIndex++)
                {
                    foreach (var rangeProfileTuple in soundSpeedProfilesAlongRadial) sspFile.Write("{0,-10:0.###}", rangeProfileTuple.Item2.Data[depthIndex].SoundSpeed);
                    sspFile.WriteLine();
                }
                //sspFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "{0,-10:0.###}{1,-10:0.###}{2,-10:0.###}", startProfile.Data[depthIndex].SoundSpeed, middleProfile.Data[depthIndex].SoundSpeed, endProfile.Data[depthIndex].SoundSpeed));
            }
            using (var trcFile = new StreamWriter(radial.BasePath + ".trc", false))
            {
                var topReflectionCoefficients = GenerateReflectionCoefficients(windSpeed, frequency);
                trcFile.WriteLine(topReflectionCoefficients.GetLength(0));
                for (var rowIndex = 0; rowIndex < topReflectionCoefficients.GetLength(0); rowIndex++)
                    trcFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "{0} {1} {2} ", topReflectionCoefficients[rowIndex, 0], topReflectionCoefficients[rowIndex, 1], topReflectionCoefficients[rowIndex, 2]));
            }
            using (var writer = new StreamWriter(radial.BasePath + ".bty")) writer.Write(bottomProfile.ToBellhopString());

            // Now that we've got the files ready to go, we can launch bellhop to do the actual calculations
            var bellhopProcess = new TransmissionLossProcess
            {
                StartInfo = new ProcessStartInfo(Path.Combine(AssemblyLocation, "bellhop.exe"), radial.Filename)
                {
                    CreateNoWindow = true,
                    UseShellExecute = false,
                    RedirectStandardInput = false,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    WorkingDirectory = directoryPath
                }
            };
            if (radial.IsDeleted) throw new RadialDeletedByUserException();
            bellhopProcess.Start();
            bellhopProcess.PriorityClass = ProcessPriorityClass.Idle;
            //bellhopProcess.BeginOutputReadLine();
            while (!bellhopProcess.HasExited)
            {
                if (radial.IsDeleted)
                {
                    bellhopProcess.Kill();
                    throw new RadialDeletedByUserException();
                }
                Thread.Sleep(20);
            }
            if (bellhopProcess.ExitCode == 0) radial.ExtractAxisData();
        }

        static readonly string AssemblyLocation = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location);
        static double[,] GenerateReflectionCoefficients(double windSpeed, double frequency, double startAngle = 0, double endAngle = 90.0, double angleStep = 1.0)
        {
            frequency /= 1000;  // Frequency is expressed in kHz in the formula

            var sampleCount = (int)((endAngle - startAngle) / angleStep) + 1;

            var result = new double[sampleCount, 3];

            var f32 = Math.Pow(frequency, 3.0 / 2.0);
            var wind4 = Math.Pow(windSpeed / 10, 4.0);
            var eta = 3.4 * f32 * wind4;
            var angle = startAngle;
            for (var angleIndex = 0; angleIndex < sampleCount; angleIndex++)
            {
                result[angleIndex, 0] = angle;
                result[angleIndex, 1] = Math.Exp(-eta * Math.Sin(angle * (Math.PI / 180.0)));
                result[angleIndex, 2] = 180;
                angle += angleStep;
            }
            return result;
        }

    }
}

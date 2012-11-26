using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Threading;
using BellhopPlugin.Controls;
using ESME;
using ESME.Environment;
using ESME.Model;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using HRC;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;

namespace BellhopPlugin
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ESMEPlugin(Name = "RAMGeo",
                Description = "RAMGeo")]
    public class RAMGeoEngine : TransmissionLossCalculatorPluginBase
    {
        [UsedImplicitly] PropertyObserver<RAMGeoEngine> _propertyObserver;
        public RAMGeoEngine()
        {
            PluginSubtype = PluginSubtype.RAMGeo;
            ConfigurationControl = new RAMGeoConfigurationControl { DataContext = this };
            Initialize();
            _propertyObserver = new PropertyObserver<RAMGeoEngine>(this)
                .RegisterHandler(p => p.MinimumOutputRangeResolution, Save)
                .RegisterHandler(p => p.MinimumOutputDepthResolution, Save);
        }

        void Initialize()
        {
            SetPropertiesFromAttributes(GetType());
            if (!File.Exists(ConfigurationFile)) Save();
            IsConfigured = true;
        }

        protected override void Save()
        {
            var serializer = new XmlSerializer<RAMGeoEngine> { Data = this };
            serializer.Save(ConfigurationFile, null);
        }

        public override void LoadSettings()
        {
            var settings = XmlSerializer<RAMGeoEngine>.LoadExistingFile(ConfigurationFile, null);
            if (settings == null) return;
            MinimumOutputRangeResolution = settings.MinimumOutputRangeResolution;
            MinimumOutputDepthResolution = settings.MinimumOutputDepthResolution;
        }

        /// <summary>
        /// Maximum depth to output TL grid, in meters
        /// This is the depth of the computational grid over which results are written to the output file
        /// A value of -1 causes the entire grid to be output subject to the minimum resolution options
        /// 
        /// zmplt
        /// </summary>
        [Initialize(-1.0)] public double MaximumOutputDepth { get; set; }

        /// <summary>
        /// RAMGEO computational depth grid spacing, in wavelengths
        /// Should be less than 0.25 (Jensen, et al.)
        /// Maximum number of points for RAMGeo is 8000
        /// 
        /// dz_lambda
        /// </summary>
        [Initialize(0.0500)] public double RelativeDepthResolution { get; set; }

        /// <summary>
        /// Minimum output depth cell resolution, in meters
        /// 
        /// dzgridmin
        /// </summary>
        [Initialize(10.0)] public double MinimumOutputDepthResolution { get; set; }

        /// <summary>
        /// RAMGEO computational range grid spacing, relative to depth resolution
        /// Should be between 2.0 - 5.0 with bottom interaction, or 20-50 without bottom interaction (Jensen, et al.)
        /// 
        /// dr_dz
        /// </summary>
        [Initialize(2.0)] public double RelativeRangeResolution { get; set; }

        /// <summary>
        /// Minimum output range cell resolution, in meters
        /// 
        /// drgridmin
        /// </summary>
        [Initialize(10.0)] public double MinimumOutputRangeResolution { get; set; }

        /// <summary>
        /// Reference sound speed, in meters per second
        /// 
        /// c0
        /// </summary>
        [Initialize(1500.0)] public double ReferenceSoundSpeed { get; set; }

        /// <summary>
        /// Number of terms in the Pade expansion (max 10)
        /// 
        /// np
        /// </summary>
        [Initialize(6)] public int PadeExpansionTerms { get; set; }

        /// <summary>
        /// Number of stability constraints
        /// 
        /// ns
        /// </summary>
        [Initialize(1)] public int StabilityConstraints { get; set; }

        /// <summary>
        /// Maximum range of stability constraints, in meters (0 = full range)
        /// 
        /// rs
        /// </summary>
        [Initialize(0)] public int StabilityConstraintMaxRange { get; set; }

        /// <summary>
        /// Attenuation layer thickness, in wavelengths
        /// 
        /// AttenLayerDz_lambda
        /// </summary>
        [Initialize(10.0)] public double AttenuationLayerThickness { get; set; }

        /// <summary>
        /// Attenuation layer maximum p-wave attenuation, in dB per wavelength
        /// 
        /// AttenLayerAttenPMax
        /// </summary>
        [Initialize(10.0)] public double AttenuationLayerMaxPWaveAttenuation { get; set; }

        /// <summary>
        /// Attenuation layer maximum s-wave attenuation, in dB per wavelength
        /// 
        /// AttenLayerAttenSMax [RAMSGeo]
        /// </summary>
        [Initialize(10.0)] public double AttenuationLayerMaxSWaveAttenuation { get; set; }

        /// <summary>
        /// Minimum shear velocity in substrate, in meters per second
        /// 
        /// CsMin [RAMSGeo]
        /// </summary>
        public double MinimumShearVelocityInSubstrate { get; set; }

        /// <summary>
        /// irot [RAMSGeo]
        /// </summary>
        public double IRot { get; set; }
        
        /// <summary>
        /// theta [RAMSGeo]
        /// </summary>
        public double Theta { get; set; }

        /// <summary>
        /// Last layer thickness, in wavelengths  
        /// 
        /// LastLayerDz_lambda
        /// </summary>
        [Initialize(10.0)] public double LastLayerThickness { get; set; }

        public override void CalculateTransmissionLoss(Platform platform, Mode mode, Radial radial, BottomProfile bottomProfile, SedimentType sedimentType, double windSpeed, IList<Tuple<double, SoundSpeedProfile>> soundSpeedProfilesAlongRadial)
        {
            var sourceDepth = platform.Depth;
            var frequency = (float)Math.Sqrt(mode.HighFrequency * mode.LowFrequency);
            if (mode.Depth.HasValue) sourceDepth += mode.Depth.Value;
            var directoryPath = Path.GetDirectoryName(radial.BasePath);
            if (directoryPath == null) throw new NullReferenceException("radial.BasePath does not point to a valid directory");
            if (!Directory.Exists(directoryPath)) Directory.CreateDirectory(directoryPath);

            // Derived Parameters
            // ==================
            var lambda = ReferenceSoundSpeed / frequency;
            // if dz < 1m round dz down to either [1/10, 1/5, 1/4 or 1/2] m  ... or mulitples of 10^-n of these numbers
            //                                  = [1     2    2.5 or 5  ] x 0.1m  "   " ...
            // if dz > 1m round dz down to either [1     2    2.5    5  ] m  ... or mulitples of 10^+n of these numbers
            var dz = RelativeDepthResolution * lambda;
            // todo: Port this function or find an equivalent dz = fix2x10pN(dz, [1 2 2.5 5 ]);
            var ndz = Math.Max(1.0, Math.Floor(MinimumOutputDepthResolution / dz));
            //  similar for dr and assoc. grid decimation
            var dr = RelativeRangeResolution * dz;
            // todo: Port this function or find an equivalent dr = fix2x10pN(dr, [1 2 2.5 5 ]);
            var ndr = Math.Max(1, Math.Floor(MinimumOutputRangeResolution / dr));
            //  attenuation layer (round up to nearest dz)
            var attenLayerDz = Math.Ceiling(AttenuationLayerThickness * lambda / dz) * dz;
            var cpMax = sedimentType.CompressionWaveSpeed;
            var lastLayerDz = LastLayerThickness * cpMax / frequency;
            lastLayerDz = Math.Ceiling(lastLayerDz / dz) * dz;
            var maxSubstrateDepth = bottomProfile.MaxDepth + lastLayerDz;
            var zmplt = maxSubstrateDepth + 2 * ndz * dz;
            // Maximum Depth for PE calc ->  zmax 
            //  zmax is the z-limit for the PE calc from top of the water column to the bottom of the last substrate layer 
            // (including the attentuation layer if, as recommended, this is included)
            var zmax = maxSubstrateDepth + attenLayerDz;

            using (var envFile = new StreamWriter(radial.BasePath + ".env", false))
            {
                envFile.WriteLine("RAMGeo");
                envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "{0}\t{1}\t{2}\t f [Frequency (Hz)], zs [Source Depth (m)], zrec0 [First receiever depth (m)]", frequency, sourceDepth, MinimumOutputDepthResolution));
                envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "{0}\t{1}\t{2}\t rmax[Max range (m)], dr [Range resolution (m)], ndr [Number of receiver ranges (1 for horizontal array)]", mode.MaxPropagationRadius, MinimumOutputRangeResolution, ndr));
                envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "{0}\t{1}\t{2}\t{3}\t zmax [Max computational depth (m)], dz [Depth resolution (m)], ndz [z grid decimation factor], zmplot [Maximum depth to plot (m)]", zmax, dz, ndz, zmplt));
                // todo: different stuff goes here for RAMSGeo

                // bathymetry data
                var first = true;
                foreach (var profilePoint in bottomProfile.Profile)
                {
                    envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "{0}\t{1}{2}", profilePoint.Range * 1000, profilePoint.Depth, first ? "\tbathymetry data [range (m), depth (m)]" : ""));
                    first = false;
                }
                envFile.WriteLine("-1\t-1");

                // range-dependent environment profiles
                var firstRangeProfile = true;
                foreach (var rangeProfileTuple in soundSpeedProfilesAlongRadial)
                {
                    // Range of profile only written for second and subsequent profiles
                    if (!firstRangeProfile) envFile.WriteLine("{0}\t\t\t\tProfile range (m)", rangeProfileTuple.Item1);

                    var firstSoundSpeedProfile = true;
                    foreach (var profilePoint in rangeProfileTuple.Item2.Data)
                    {
                        envFile.WriteLine("{0}\t{1}{2}", profilePoint.Depth, profilePoint.SoundSpeed, firstSoundSpeedProfile ? "\t\t\tsound speed profile in water [depth (m), sound speed (m/s)]" : "");
                        firstSoundSpeedProfile = false;
                    }
                    envFile.WriteLine("-1\t-1");

                    // todo: RAMGeo and RAMSGeo also support sediment layers, as well as range-dependent sediment, neither of which is not yet supported by ESME
                    // If sediment layers are ever supported, put a loop like for the sound speed profile above
                    // A sediment layer is analogous to a sound speed profile point
                    // For range-dependent sediment, the sediment samples have to be at the same ranges as the sound speed profiles
                    // so we might want to change the API to include sediment properties in what is the current range and sound speed profile tuple
                    envFile.WriteLine("{0}\t{1}\t\t\tcompressive sound speed profile in substrate [depth (m), sound speed (m/s)]", 5.0, sedimentType.CompressionWaveSpeed);
                    envFile.WriteLine("-1\t-1");
                    envFile.WriteLine("{0}\t{1}\t\t\tdensity profile in substrate [depth (m), rhob (g/cm³)]", 5.0, sedimentType.Density);
                    envFile.WriteLine("-1\t-1");
                    envFile.WriteLine("{0}\t{1}\t\t\tcompressive attenuation profile in substrate [depth (m), attnp (db/lambda)]", 5.0, 0.0);
                    envFile.WriteLine("-1\t-1");
                    firstRangeProfile = false;
                }
            }

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
            bellhopProcess.BeginOutputReadLine();
            while (!bellhopProcess.HasExited)
            {
                if (radial.IsDeleted)
                {
                    bellhopProcess.Kill();
                    throw new RadialDeletedByUserException();
                }
                Thread.Sleep(20);
            }
        }

        static readonly string AssemblyLocation = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location);

    }
}

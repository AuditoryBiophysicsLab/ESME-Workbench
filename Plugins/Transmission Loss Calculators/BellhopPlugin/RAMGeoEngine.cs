using System;
using System.Linq;
using System.Numerics;
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
using ESME.NEMO;
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
        [UsedImplicitly]
        PropertyObserver<RAMGeoEngine> _propertyObserver;
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
        [Initialize(-1.0)]
        public double MaximumOutputDepth { get; set; }

        /// <summary>
        /// RAMGEO computational depth grid spacing, in wavelengths
        /// Should be less than 0.25 (Jensen, et al.)
        /// Maximum number of points for RAMGeo is 8000
        /// 
        /// dz_lambda
        /// </summary>
        [Initialize(0.1)]
        public double RelativeDepthResolution { get; set; }

        /// <summary>
        /// Minimum output depth cell resolution, in meters
        /// 
        /// dzgridmin
        /// </summary>
        [Initialize(10.0)]
        public double MinimumOutputDepthResolution { get; set; }

        /// <summary>
        /// RAMGEO computational range grid spacing, relative to depth resolution
        /// Should be between 2.0 - 5.0 with bottom interaction, or 20-50 without bottom interaction (Jensen, et al.)
        /// 
        /// dr_dz
        /// </summary>
        [Initialize(2.0)]
        public double RelativeRangeResolution { get; set; }

        /// <summary>
        /// Minimum output range cell resolution, in meters
        /// 
        /// drgridmin
        /// </summary>
        [Initialize(10.0)]
        public double MinimumOutputRangeResolution { get; set; }

        /// <summary>
        /// Reference sound speed, in meters per second
        /// 
        /// c0
        /// </summary>
        [Initialize(1500.0)]
        public double ReferenceSoundSpeed { get; set; }

        /// <summary>
        /// Number of terms in the Pade expansion (max 10)
        /// 
        /// np
        /// </summary>
        [Initialize(6)]
        public int PadeExpansionTerms { get; set; }

        /// <summary>
        /// Number of stability constraints
        /// 
        /// ns
        /// </summary>
        [Initialize(1)]
        public int StabilityConstraints { get; set; }

        /// <summary>
        /// Maximum range of stability constraints, in meters (0 = full range)
        /// 
        /// rs
        /// </summary>
        [Initialize(0)]
        public int StabilityConstraintMaxRange { get; set; }

        /// <summary>
        /// Attenuation layer thickness, in wavelengths
        /// 
        /// AttenLayerDz_lambda
        /// </summary>
        [Initialize(10.0)]
        public double AttenuationLayerThickness { get; set; }

        /// <summary>
        /// Attenuation layer maximum p-wave attenuation, in dB per wavelength
        /// 
        /// AttenLayerAttenPMax
        /// </summary>
        [Initialize(10.0)]
        public double AttenuationLayerMaxPWaveAttenuation { get; set; }

        /// <summary>
        /// Attenuation layer maximum s-wave attenuation, in dB per wavelength
        /// 
        /// AttenLayerAttenSMax [RAMSGeo]
        /// </summary>
        [Initialize(10.0)]
        public double AttenuationLayerMaxSWaveAttenuation { get; set; }

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
        [Initialize(10.0)]
        public double LastLayerThickness { get; set; }

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
            // if dz < 1m round dz down to either [1/10, 1/5, 1/4 or 1/2] m  ... or multiples of 10^-n of these numbers
            //                                  = [1     2    2.5 or 5  ] x 0.1m  "   " ...
            // if dz > 1m round dz down to either [1     2    2.5    5  ] m  ... or multiples of 10^+n of these numbers
            var fixpoints = new List<double>{1, 2, 2.5, 5};
            var dz = RelativeDepthResolution * lambda;
            dz = Fix2X10pN(dz, fixpoints);

            var ndz = (int)Math.Max(1.0, Math.Floor(MinimumOutputDepthResolution / dz));
            //var ndz = (int)Math.Max(1.0, Math.Floor(MinimumOutputDepthResolution / lambda));
            //ndz = 1;
            //  similar for dr and assoc. grid decimation
            var dr = RelativeRangeResolution * dz;
            dr = Fix2X10pN(dr, fixpoints);

            var ndr = (int)Math.Max(1, Math.Floor(MinimumOutputRangeResolution / dr));
            //var ndr = (int)Math.Max(1, Math.Floor(MinimumOutputRangeResolution / lambda));
            //ndr = 1;
            //  attenuation layer (round up to nearest dz)
            var sedimentLambda = sedimentType.CompressionWaveSpeed / frequency;
            //var attenLayerDz = Math.Ceiling(AttenuationLayerThickness * lambda / dz) * dz;
            var attenLayerDz = Math.Ceiling(AttenuationLayerThickness * sedimentLambda / dz) * dz;
            var cpMax = sedimentType.CompressionWaveSpeed;
            var lastLayerDz = LastLayerThickness * cpMax / frequency;
            lastLayerDz = Math.Ceiling(lastLayerDz / dz) * dz;
            var maxSubstrateDepth = bottomProfile.MaxDepth + lastLayerDz;
            var zmplt = maxSubstrateDepth + 2 * ndz * dz;
            // Maximum Depth for PE calc ->  zmax 
            //  zmax is the z-limit for the PE calc from top of the water column to the bottom of the last substrate layer 
            // (including the attentuation layer if, as recommended, this is included)
            var zmax = maxSubstrateDepth + attenLayerDz * 2;
            var envFileName = radial.BasePath + ".env";
            using (var envFile = new StreamWriter(envFileName, false))
            {
                envFile.WriteLine("RAMGeo");
                envFile.WriteLine("{0:0.000000}\t{1:0.000000}\t{2:0.000000}\t\tf [Frequency (Hz)], zs [Source Depth (m)], zrec0 [First receiever depth (m)]", frequency, sourceDepth, MinimumOutputDepthResolution);
                envFile.WriteLine("{0:0.000000}\t{1:0.000000}\t{2}\t\t\trmax[Max range (m)], dr [Range resolution (m)], ndr [Range grid decimation factor]", mode.MaxPropagationRadius, MinimumOutputRangeResolution, ndr);
                envFile.WriteLine("{0:0.000000}\t{1:0.000000}\t{2}\t{3:0.000000}\tzmax [Max computational depth (m)], dz [Depth resolution (m)], ndz [Depth grid decimation factor], zmplot [Maximum depth to plot (m)]", zmax, dz, ndz, zmplt);
                envFile.WriteLine("{0:0.000000}\t{1}\t{2}\t{3:0.000000}\t\tc0 [Reference sound speed (m/s)], np [Number of terms in Padé expansion], ns [Number of stability constraints], rs [Maximum range of stability constraints (m)]", ReferenceSoundSpeed, PadeExpansionTerms, StabilityConstraints, StabilityConstraintMaxRange);
                // todo: different stuff goes here for RAMSGeo

                // bathymetry data
                var first = true;
                foreach (var profilePoint in bottomProfile.Profile)
                {
                    envFile.WriteLine(string.Format(CultureInfo.InvariantCulture, "{0:0.000000}\t{1:0.000000}{2}", profilePoint.Range * 1000, profilePoint.Depth, first ? "\t\t\t\t\tbathymetry data [range (m), depth (m)]" : ""));
                    first = false;
                }
                envFile.WriteLine("-1\t-1");

                // range-dependent environment profiles
                var firstRangeProfile = true;
                foreach (var rangeProfileTuple in soundSpeedProfilesAlongRadial)
                {
                    // Range of profile only written for second and subsequent profiles
                    if (!firstRangeProfile) envFile.WriteLine("{0:0.#}\t\t\t\t\t\t\tProfile range (m)", rangeProfileTuple.Item1 * 1000);

                    var firstSoundSpeedProfile = true;
                    foreach (var profilePoint in rangeProfileTuple.Item2.Data)
                    {
                        if (double.IsNaN(profilePoint.SoundSpeed)) break;
                        envFile.WriteLine("{0:0.######}\t{1:0.######}{2}", profilePoint.Depth, profilePoint.SoundSpeed, firstSoundSpeedProfile ? "\t\t\t\t\tsound speed profile in water [depth (m), sound speed (m/s)]" : "");
                        firstSoundSpeedProfile = false;
                    }
                    envFile.WriteLine("-1\t-1");

                    // todo: RAMGeo and RAMSGeo also support sediment layers, as well as range-dependent sediment, neither of which is not yet supported by ESME
                    // If sediment layers are ever supported, put a loop like for the sound speed profile above
                    // A sediment layer is analogous to a sound speed profile point
                    // For range-dependent sediment, the sediment samples have to be at the same ranges as the sound speed profiles
                    // so we might want to change the API to include sediment properties in what is the current range and sound speed profile tuple
                    envFile.WriteLine("{0:0.######}\t{1:0.######}\t\t\t\t\t\tcompressive sound speed profile in substrate [depth (m), sound speed (m/s)]", 0.0, sedimentType.CompressionWaveSpeed);
                    envFile.WriteLine("-1\t-1");
                    envFile.WriteLine("{0:0.######}\t{1:0.######}\t\t\t\t\t\tdensity profile in substrate [depth (m), rhob (g/cm³)]", 0.0, sedimentType.Density);
                    envFile.WriteLine("-1\t-1");
                    envFile.WriteLine("{0:0.######}\t{1:0.######}\t\t\t\t\t\tcompressive attenuation profile in substrate [depth (m), attnp (db/lambda)]", 0.0, 0.0);
                    envFile.WriteLine("{0:0.######}\t{1:0.######}", attenLayerDz, 40);
                    envFile.WriteLine("-1\t-1");
                    firstRangeProfile = false;
                }
            }
            var tempDirectory = Path.Combine(Path.GetTempPath(), Path.GetFileNameWithoutExtension(envFileName));
            //Debug.WriteLine(string.Format("Env File: {0} temp path: {1}", envFileName, tempDirectory));
            if (Directory.Exists(tempDirectory)) Directory.Delete(tempDirectory, true);
            if (File.Exists(tempDirectory)) File.Delete(tempDirectory);
            Directory.CreateDirectory(tempDirectory);
            File.Copy(envFileName, Path.Combine(tempDirectory, "ramgeo.in"));
            //Debug.WriteLine(string.Format("Env File: {0} copied to: {1}", envFileName, tempDirectory));
            // Now that we've got the files ready to go, we can launch bellhop to do the actual calculations
            var ramProcess = new TransmissionLossProcess
            {
                StartInfo = new ProcessStartInfo(Path.Combine(AssemblyLocation, "RAMGeo.exe"))
                {
                    CreateNoWindow = true,
                    UseShellExecute = false,
                    RedirectStandardInput = false,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    WorkingDirectory = tempDirectory
                }
            };
            if (radial.IsDeleted) throw new RadialDeletedByUserException();
            ramProcess.Start();
            ramProcess.PriorityClass = ProcessPriorityClass.Idle;
            //ramProcess.BeginOutputReadLine();
            while (!ramProcess.HasExited)
            {
                if (radial.IsDeleted)
                {
                    ramProcess.Kill();
                    throw new RadialDeletedByUserException();
                }
                Thread.Sleep(20);
            }
            var ramError = ramProcess.StandardError.ReadToEnd();
            if (ramProcess.ExitCode == 0)
            {
                File.Delete(Path.Combine(tempDirectory, "ramgeo.in"));
                File.Delete(radial.BasePath + ".grid");
                File.Move(Path.Combine(tempDirectory, "tl.grid"), radial.BasePath + ".grid");
                File.Delete(radial.BasePath + ".line");
                File.Move(Path.Combine(tempDirectory, "tl.line"), radial.BasePath + ".line");
                File.Delete(radial.BasePath + ".pgrid");
                File.Move(Path.Combine(tempDirectory, "p.grid"), radial.BasePath + ".pgrid");
                File.Delete(radial.BasePath + ".pline");
                File.Move(Path.Combine(tempDirectory, "p.line"), radial.BasePath + ".pline");

                using (var writer = new StreamWriter(radial.BasePath + ".bty")) writer.Write(bottomProfile.ToBellhopString());
                var pressures = ReadRamPGrid(radial.BasePath + ".pgrid");
                var rangeCount = pressures.Count;
                var depthCount = pressures[0].Length;
                var dzplt = dz * ndz;
                var drplt = dr * ndr;
                var rr = new double[rangeCount];
                for (var rangeIndex = 0; rangeIndex < rr.Length; rangeIndex++) rr[rangeIndex] = (rangeIndex + 1) * drplt;
                var rd = new double[depthCount];
                for (var depthIndex = 0; depthIndex < rd.Length; depthIndex++) rd[depthIndex] = (depthIndex + 1) * dzplt;
                BellhopOutput.WriteShadeFile(radial.BasePath + ".shd", sourceDepth, frequency, rd, rr, pressures);
                //WritePGridCompare(path+".reals",result,true);
                //WritePGridCompare(path + ".imags", result, false);
            }
            else
            {
                Debug.WriteLine("RAMGeo process for radial {0} exited with error code {1:X}", radial.BasePath, ramProcess.ExitCode);
                Debug.WriteLine(ramError);
            }
            Directory.Delete(tempDirectory, true);
            //Debug.WriteLine(string.Format("Env File: {0} temp directory deleted: {1}", envFileName, tempDirectory));
        }

        static readonly string AssemblyLocation = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location);

#if false
        static void WritePGridCompare(string fileName, List<Complex[]> pgrid, bool writeReal)
        {
            using (var writer = new StreamWriter(fileName))
            {
                var rangeCount = pgrid.Count();
                var depthCount = pgrid[0].Length;
                for (var i = 0; i < depthCount; i++)
                {
                    for (var j = 0; j < rangeCount; j++)
                    {
                        var complex = pgrid[j][i];
                        writer.Write(string.Format("{0} ", writeReal ? complex.Real : complex.Imaginary));
                    }
                    writer.WriteLine();
                }
            }
        }
#endif

        static double Fix2X10pN(double x, List<double> fixpoints)
        {
            fixpoints.Sort();
            if(fixpoints.First() < 1) throw new ParameterOutOfRangeException("No negative numbers.");
            if(Math.Abs(fixpoints.First() - 1) > .001) fixpoints.Insert(0,1);
            if(fixpoints.Last()>10) throw new ParameterOutOfRangeException("Fixpoints  must be between 1 and 10");

            var px = Math.Floor(Math.Log10(x));
            var fx = x * Math.Pow(10, -px);

            var done = false;
            var ii = 2;
            var n = fixpoints.Count;

            while (!done)
            {
                if (fx < fixpoints[ii])
                {
                    fx = fixpoints[ii - 1];
                    done = true;
                }
                else if (ii == n)
                {
                    fx = fixpoints.Last();
                    done = true;
                }
                ii++;
            }
            return (fx * Math.Pow(10,px));
        }


        //object _lockObject = new object();
        public List<Complex[]> ReadRamPGrid(string fileName)
        {
            using (var reader = new BinaryReader(new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)))
            {
                reader.ReadUInt32(); 

                var numDepths = reader.ReadInt32();
                var recLen = numDepths;
                if (numDepths > 0)
                {
                    var doneAll = false;
                    var pGrid = new List<Complex[]>(); 

                    while (!doneAll)
                    {
                        var pColumn = new Complex[numDepths]; 
                        var doneCol = false;
                        var startSub = 1;

                        while (!doneCol && !doneAll)
                        {
                            var endSub = startSub + 2 * recLen - 1;
                            if (endSub > 2 * numDepths)
                            {
                                endSub = 2 * numDepths;
                                doneCol = true;
                            }
                            var nRead = ((endSub - startSub) + 1) / 2;
                            if (nRead > 0)
                            {
                                try
                                {
                                    reader.ReadUInt32();
                                    reader.ReadUInt32();
                                    for (var i = 0; i < nRead; i++) pColumn[i] = new Complex(reader.ReadSingle(), reader.ReadSingle());
                                    startSub = endSub + 1;
                                }
                                catch (Exception)
                                {
                                    // premature eof or other problem?
                                    doneAll = true;
                                }
                            }
                            else doneCol = true;
                        }
                        if (!doneAll) pGrid.Add(pColumn);
                    }
                    //loop's done, write it out and return it;
                    return pGrid;
                }
                return null;
            }
        }
    }
}

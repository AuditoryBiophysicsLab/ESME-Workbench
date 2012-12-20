using System;
using System.Linq;
using System.Numerics;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Text;
using System.Threading;
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
using MathNet.Numerics.IntegralTransforms;
using StandardTransmissionLossEngines.Controls;

namespace StandardTransmissionLossEngines
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
        /// Attenuation factor, in dB for side lobes of the beam pattern
        /// </summary>
        [Initialize(40.0)] public double SideLobeAttenuation { get; set; }
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
            // Note: All the specific calculations given in the comments below assume a frequency of 1kHz
            // lambda is wavelength, in meters
            var lambda = ReferenceSoundSpeed / frequency;
            // if dz < 1m round dz down to either [1/10, 1/5, 1/4 or 1/2] m  ... or multiples of 10^-n of these numbers
            //                                  = [1     2    2.5 or 5  ] x 0.1m  "   " ...
            // if dz > 1m round dz down to either [1     2    2.5    5  ] m  ... or multiples of 10^+n of these numbers
            var fixpoints = new List<double> { 1, 2, 2.5, 5 };
            // dz = 0.1 * lambda
            var dz = RelativeDepthResolution * lambda;
            // make dz a 'pretty' number
            //dz = Fix2X10pN(dz, fixpoints);

            // ndz is the depth decimation factor
            // MinimumOutputDepthResolution is 10m
            // dz is 0.1 * lambda (dz = 0.15 for a 1 kHz signal, 'pretty' dz = 0.2 @ 1kHz)
            // so ndz = (10 / 0.2) = 50 @ 1kHz
            // this means that we will only output every 50 computational depth cells, giving us a depth
            // resolution of 50 * 0.2m = 10m @ 1kHz which is what we want it to be.  Outstanding.
            var ndz = (int)Math.Max(1.0, Math.Floor(MinimumOutputDepthResolution / dz));

            //  similar for dr and assoc. grid decimation
            // RelativeRangeResolution is 2, so with our 'pretty' dz = 0.2, dr = 0.4
            var dr = RelativeRangeResolution * dz;
            // make dr a 'pretty' number, in this case 0.25
            //dr = Fix2X10pN(dr, fixpoints);
            // ndr is the range decimation factor
            // MinimumOutputRangeResolution is 10m
            // dr is 0.25 * lambda, so (10 / 0.25) gives us an ndr of 40
            // this means that we will only output every 40 computational range cells, giving us a range
            // resolution of 40 * 0.25m = 10m @ 1kHz which is what we want it to be.  Outstanding.
            var ndr = (int)Math.Max(1, Math.Floor(MinimumOutputRangeResolution / dr));

            //  attenuation layer (round up to nearest dz)
            var sedimentLambda = sedimentType.CompressionWaveSpeed / frequency;
            var sedimentLayerDz = Math.Ceiling(LastLayerThickness * sedimentLambda / dz) * dz;
            var attenuationLayerDz = Math.Ceiling(AttenuationLayerThickness * sedimentLambda / dz) * dz;
            var maxSubstrateDepth = bottomProfile.MaxDepth + sedimentLayerDz;
            var zstep = dz * ndz;
            var zmplt = Math.Ceiling((bottomProfile.MaxDepth + 2 * zstep) / zstep) * zstep;
            // Maximum Depth for PE calc ->  zmax 
            //  zmax is the z-limit for the PE calc from top of the water column to the bottom of the last substrate layer 
            // (including the attentuation layer if, as recommended, this is included)
            var zmax = maxSubstrateDepth + attenuationLayerDz;
            var envFileName = radial.BasePath + ".env";
            Debug.WriteLine("Scenario: '{0}' Mode: '{2}' Analysis point: {1} Bearing: {3}, zmplt: {4}",
                            radial.TransmissionLoss.AnalysisPoint.Scenario.Name,
                            radial.TransmissionLoss.AnalysisPoint.Geo,
                            radial.TransmissionLoss.Modes[0].ModeName,
                            radial.Bearing, zmplt);

            using (var envFile = new StreamWriter(envFileName, false))
            {
                envFile.WriteLine("Scenario: '{0}' Mode: '{2}' Analysis point: {1} Bearing: {3}",
                                  radial.TransmissionLoss.AnalysisPoint.Scenario.Name,
                                  radial.TransmissionLoss.AnalysisPoint.Geo,
                                  radial.TransmissionLoss.Modes[0].ModeName,
                                  radial.Bearing);
                envFile.WriteLine("{0:0.000000}\t{1:0.000000}\t{2:0.000000}\t\tf [Frequency (Hz)], zs [Source Depth (m)], zrec0 [First receiever depth (m)]",
                                  frequency,
                                  sourceDepth,
                                  0.1);
                envFile.WriteLine("{0:0.000000}\t{1:0.000000}\t{2}\t\t\trmax[Max range (m)], dr [Range resolution (m)], ndr [Range grid decimation factor]",
                                  mode.MaxPropagationRadius + (dr * ndr),
                                  dr,
                                  ndr);
                envFile.WriteLine("{0:0.000000}\t{1:0.000000}\t{2}\t{3:0.000000}\tzmax [Max computational depth (m)], dz [Depth resolution (m)], ndz [Depth grid decimation factor], zmplot [Maximum depth to plot (m)]",
                                  zmax,
                                  dz,
                                  ndz,
                                  zmplt);
                envFile.WriteLine("{0:0.000000}\t{1}\t{2}\t{3:0.000000}\t\tc0 [Reference sound speed (m/s)], np [Number of terms in Padé expansion], ns [Number of stability constraints], rs [Maximum range of stability constraints (m)]",
                                  ReferenceSoundSpeed,
                                  PadeExpansionTerms,
                                  StabilityConstraints,
                                  StabilityConstraintMaxRange);
                // todo: different stuff goes here for RAMSGeo

                // bathymetry data
                var first = true;
                foreach (var profilePoint in bottomProfile.Profile)
                {
                    envFile.WriteLine(string.Format(CultureInfo.InvariantCulture,
                                                    "{0:0.000000}\t{1:0.000000}{2}",
                                                    profilePoint.Range * 1000,
                                                    profilePoint.Depth,
                                                    first ? "\t\t\t\t\tbathymetry data [range (m), depth (m)]" : ""));
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
                        envFile.WriteLine("{0:0.######}\t{1:0.######}{2}",
                                          profilePoint.Depth,
                                          profilePoint.SoundSpeed,
                                          firstSoundSpeedProfile ? "\t\t\t\t\tsound speed profile in water [depth (m), sound speed (m/s)]" : "");
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
                    envFile.WriteLine("{0:0.######}\t{1:0.######}", attenuationLayerDz, 40);
                    envFile.WriteLine("-1\t-1");
                    firstRangeProfile = false;
                }
            }
            var tempDirectory = Path.Combine(Path.GetTempPath(), Path.GetFileNameWithoutExtension(envFileName));
            //Debug.WriteLine(string.Format("Env File: {0} temp path: {1}", envFileName, tempDirectory));
            if (Directory.Exists(tempDirectory))
            {
                var files = Directory.GetFiles(tempDirectory, "*.*");
                foreach (var file in files) File.Delete(file);
                Directory.Delete(tempDirectory, true);
            } else if (File.Exists(tempDirectory)) File.Delete(tempDirectory);
            Directory.CreateDirectory(tempDirectory);
            File.Copy(envFileName, Path.Combine(tempDirectory, "ramgeo.in"));
            using (var steerableArrayFile = new StreamWriter(Path.Combine(tempDirectory, "sra.in"), false))
            {
                // From http://www.activefrance.com/Antennas/Introduction%20to%20Phased%20Array%20Design.pdf
                // theta3 = 3dB beam width, in degrees
                // emitterSize = size of emitter array, in meters
                // theta3 = (0.886 * lambda / arrayLength) * 180 / pi
                // so, doing the algebra and solving for arrayLength, you get:
                // emitterSize = (0.886 * lambda) / (theta3 * (pi / 180))
                var emitterSize = (0.886 * lambda) / (mode.VerticalBeamWidth * (Math.PI / 180.0));
                var emitterCount = (int)(emitterSize / (dz * 2));
                var emitterSpacing = 1.0;
                var weights = new List<double> { 1 };
                if (emitterCount > 1)
                {
                    emitterSpacing = emitterSize / (emitterCount - 1);
                    // chebyshev window calculations for relative emitter strength across the array
                    var discreteFourierTransform = new MathNet.Numerics.IntegralTransforms.Algorithms.DiscreteFourierTransform();
                    var r0 = Math.Pow(10, SideLobeAttenuation / 20);
                    var n = emitterCount - 1;
                    var a = Complex.Cosh((1.0 / n) * Acosh(r0));
                    var am = new Complex[n];
                    for (var m = 0; m < n; m++) am[m] = a * Complex.Cos(Math.PI * m / n);
                    var wm = new Complex[n];
                    var sign = 1;
                    for (var i = 0; i < n; i++)
                    {
                        if (am[i].Magnitude > 1) wm[i] = sign * Complex.Cosh(n * Acosh(am[i]));
                        else wm[i] = sign * Complex.Cos(n * Complex.Acos(am[i]));
                        sign *= -1;
                    }
                    discreteFourierTransform.BluesteinInverse(wm, FourierOptions.Default);
                    weights = wm.Select(e => e.Real).ToList();
                    weights[0] /= 2;
                    weights.Add(weights[0]);
                    var maxWeight = weights.Max();
                    for (var i = 0; i < weights.Count; i++) weights[i] /= maxWeight;
                }
                steerableArrayFile.WriteLine("{0}\t{1}\t{2}", emitterCount, emitterSpacing, mode.DepressionElevationAngle);
                for (var i = 0; i < emitterCount; i++) steerableArrayFile.WriteLine("{0}", weights[i]);
            }
            //File.Copy(Path.Combine(AssemblyLocation, "sra.in"), Path.Combine(tempDirectory, "sra.in"));
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
            //File.Delete(Path.Combine(tempDirectory, "ramgeo.in"));
            //File.Delete(radial.BasePath + ".grid");
            //File.Move(Path.Combine(tempDirectory, "tl.grid"), radial.BasePath + ".grid");
            //File.Delete(radial.BasePath + ".line");
            //File.Move(Path.Combine(tempDirectory, "tl.line"), radial.BasePath + ".line");
            //File.Delete(radial.BasePath + ".pgrid");
            //File.Move(Path.Combine(tempDirectory, "p.grid"), radial.BasePath + ".pgrid");
            //File.Delete(radial.BasePath + ".sra");
            //File.Move(Path.Combine(tempDirectory, "sra.in"), radial.BasePath + ".sra");

            using (var writer = new StreamWriter(radial.BasePath + ".bty")) writer.Write(bottomProfile.ToBellhopString());
            if (File.Exists(Path.Combine(tempDirectory, "p.grid")))
            {
                var pressures = ReadPGrid(Path.Combine(tempDirectory, "p.grid"));
                File.Copy(Path.Combine(tempDirectory, "p.grid"), radial.BasePath + ".pgrid");
                //File.Delete(radial.BasePath + ".pgrid");
                var rangeCount = pressures.Count;
                var depthCount = pressures[0].Length;
                var rr = new double[rangeCount];
                var rd = new double[depthCount];
                for (var rangeIndex = 0; rangeIndex < rr.Length; rangeIndex++) rr[rangeIndex] = (rangeIndex + 1) * dr * ndr;
                for (var depthIndex = 0; depthIndex < rd.Length; depthIndex++) rd[depthIndex] = depthIndex * zstep;
                Debug.WriteLine("Scenario: '{0}' Mode: '{2}' Analysis point: {1} Bearing: {3}, zmplt: {4}, zstep: {5}, maxDepth: {6}, fileName: {7}, reqDepthCells: {8}, actualDepthCells: {9}",
                                radial.TransmissionLoss.AnalysisPoint.Scenario.Name,
                                radial.TransmissionLoss.AnalysisPoint.Geo,
                                radial.TransmissionLoss.Modes[0].ModeName,
                                radial.Bearing,
                                zmplt,
                                zstep,
                                rd.Last(),
                                Path.GetFileNameWithoutExtension(radial.BasePath),
                                zmplt / zstep,
                                depthCount);
                BellhopOutput.WriteShadeFile(radial.BasePath + ".shd", sourceDepth, frequency, rd, rr, pressures);
            }
            else
            {
                Debug.WriteLine("Scenario: {0} Analysis point: {1} Mode {2} Bearing {3}",
                                radial.TransmissionLoss.AnalysisPoint.Scenario.Name,
                                radial.TransmissionLoss.AnalysisPoint.Geo,
                                radial.TransmissionLoss.Modes[0].ModeName,
                                radial.Bearing);
                Debug.WriteLine("p.grid file not found in RAMGeo output directory");
                if (ramProcess.ExitCode != 0)
                {
                    Debug.WriteLine("RAMGeo process for radial {0} exited with error code {1:X}", radial.BasePath, ramProcess.ExitCode);
                    Debug.WriteLine(ramError);
                }
            }
            Directory.Delete(tempDirectory, true);
            //Debug.WriteLine(string.Format("Env File: {0} temp directory deleted: {1}", envFileName, tempDirectory));
        }

        static Complex Acosh(Complex x) { return Complex.Log(x + Complex.Sqrt(x + 1) * Complex.Sqrt(x - 1)); }

        static readonly string AssemblyLocation = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location);

        static List<Complex[]> ReadPGrid(string fileName)
        {
            var pGrid = new List<Complex[]>();
            try
            {
                using (var reader = new BinaryReader(new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)))
                {
                    // Toss the first 4 bytes
                    reader.ReadUInt32();

                    var numDepths = reader.ReadInt32();
                    if (numDepths <= 0) return null;

                    // Toss the next 4 bytes
                    reader.ReadUInt32();
                    while (true)
                    {
                        // This is the record header, should be the number of bytes in the record
                        var recordHeader = reader.ReadUInt32();
                        // The record header should be the number of depths * 8 (4 bytes per float, 2 floats per complex number)
                        if (recordHeader != numDepths * 8) throw new FormatException("Record header does not match number of depths");
                        var depthCells = new Complex[numDepths];
                        for (var depthIndex = 0; depthIndex < numDepths; depthIndex++) depthCells[depthIndex] = new Complex(reader.ReadSingle(), reader.ReadSingle());
                        pGrid.Add(depthCells);
                        // This is the record header, should be the number of bytes in the record
                        var recordFooter = reader.ReadUInt32();
                        // The record footer should be the number of depths * 8 (4 bytes per float, 2 floats per complex number)
                        if (recordFooter != numDepths * 8) throw new FormatException("Record footer does not match number of depths");
                    }
                }
            }
            catch (EndOfStreamException)
            {
                return pGrid;
            }
        }

        List<Complex[]> ReadRamPGrid(string fileName)
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

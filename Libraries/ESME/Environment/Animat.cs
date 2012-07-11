using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using ESME.Model;
using ESME.Scenarios;
using HRC.Navigation;
using mbs;
using FileFormatException = ESME.Model.FileFormatException;

namespace ESME.Environment
{
    public class Animat : EnvironmentDataSetBase
    {
        public Animat() { Locations = new AnimatEnvironmentData<Geo<float>>(); }
        public ScenarioSpecies ScenarioSpecies { get; set; }
        public AnimatEnvironmentData<Geo<float>> Locations { get; protected set; }
        public int TotalAnimats { get; internal set; }
        static readonly Random Random = new Random();
        public static Animat Load(ScenarioSpecies species, string fileName)
        {
            var extension = Path.GetExtension(fileName);
            if (extension != null)
                switch (extension.ToLower())
                {
                    case ".3mb":
                        return Animat3MBFile.Load(species, fileName);
                    case ".ddb":
                        return AnimatDDBFile.Load(species, fileName);
                    case ".ani":
                        using (var stream = new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read))
                        using (var reader = new BinaryReader(stream))
                        {
                            var header = reader.ReadBytes(4);
                            if (header[0] != 'a' && header[1] != 'n' && header[2] != 'i' && header[3] != 'm') throw new FileFormatException(string.Format("{0} is not a valid .ani file", fileName));
                            //var latinName = reader.ReadString();
                            //if (latinName != species.LatinName) throw new FileFormatException(string.Format("{0}: expected species name {1}, got {2}", fileName, species.LatinName, latinName));
                            var locationCount = reader.ReadInt32();
                            var result = new Animat();
                            for (var i = 0; i < locationCount; i++) result.Locations.Add(new Geo<float>(reader.ReadDouble(), reader.ReadDouble(), reader.ReadSingle()));
                            return result;
                        }

                    default:
                        throw new FileFormatException(string.Format("Unable to load animat locations.  Unrecognized file type: \"{0}\"", extension));
                }
            throw new Exception("specified file does not exist!");
        }

        public static Animat Seed(ScenarioSpecies species, GeoRect geoRect, Bathymetry bathymetry)
        {
            var bounds = new GeoArray(geoRect.NorthWest, geoRect.NorthEast, geoRect.SouthEast, geoRect.SouthWest, geoRect.NorthWest);
            var result = new Animat { ScenarioSpecies = species };
            var radius = Planet.wgs84_earthEquatorialRadiusMeters_D / 1000;
            var area = bounds.Area * radius * radius;
            //Debug.WriteLine("Area: {0}",area);
            var population = (int)Math.Floor(area * species.PopulationDensity);
            for (var i = 0; i < population; i++)
            {
                var location = bounds.RandomLocationWithinPerimeter();
                var depth = bathymetry.Samples.GetNearestPoint(location).Data;
                if (depth < 0) result.Locations.Add(new Geo<float>(location.Latitude, location.Longitude, (float)(depth * Random.NextDouble())));
            }
            result.TotalAnimats = population;
            return result;
        }

        public async static Task<Animat> SeedAsyncWithout3MB(ScenarioSpecies species, GeoRect geoRect, Bathymetry bathymetry)
        {
            var bounds = new GeoArray(geoRect.NorthWest, geoRect.NorthEast, geoRect.SouthEast, geoRect.SouthWest, geoRect.NorthWest);
            var result = new Animat { ScenarioSpecies = species };
            var radius = Planet.wgs84_earthEquatorialRadiusMeters_D / 1000;
            var area = bounds.Area * radius * radius;
            //Debug.WriteLine("Area: {0}",area);
            var transformManyBlock = new TransformManyBlock<int, Geo<float>>(count =>
            {
                var geos = new List<Geo<float>>();
                for (var i = 0; i < count; i++)
                {
                    var location = bounds.RandomLocationWithinPerimeter();
                    var depth = bathymetry.Samples.GetNearestPointAsync(location).Result.Data;
                    if (depth < -50) geos.Add(new Geo<float>(location.Latitude, location.Longitude, (float)(depth * Random.NextDouble())));
                }
                return geos;
            }, new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                BoundedCapacity = -1,
                MaxDegreeOfParallelism = -1,
            });
            var bufferBlock = new BufferBlock<Geo<float>>();
            transformManyBlock.LinkTo(bufferBlock);
            var population = (int)Math.Round(area * species.PopulationDensity);
            result.TotalAnimats = population;
            const int blockSize = 100;
            while (population > 0)
            {
                transformManyBlock.Post(population > blockSize ? blockSize : population);
                population -= blockSize;
            }
            transformManyBlock.Complete();
            await transformManyBlock.Completion;
            IList<Geo<float>> animatGeos;
            if (bufferBlock.TryReceiveAll(out animatGeos))
                result.Locations.AddRange(animatGeos);
            return result;
        }

        public async static Task<Animat> SeedAsync(ScenarioSpecies species, GeoRect geoRect, Bathymetry bathymetry)
        {
            var yxzFileName = Path.Combine(Path.GetTempPath(), Path.GetFileNameWithoutExtension(Path.GetRandomFileName()) + ".txt");
            bathymetry.ToYXZ(yxzFileName, -1);
            var mbs = new C3mbs();
            mbsRESULT mbsResult;
            if (mbsRESULT.OK != (mbsResult = mbs.SetOutputDirectory(Path.GetTempPath())))
                throw new AnimatInterfaceMMBSException("SetOutputDirectory Error:" + mbs.ResultToTc(mbsResult));
            var config = mbs.GetConfiguration();
            config.enabled = false;             // binary output enabled/disabled
            config.durationLess = true;         // make sure we're in durationless mode.
            mbs.SetConfiguration(config);
            mbsResult = mbs.LoadBathymetryFromTextFile(yxzFileName);
            if (mbsRESULT.OK != mbsResult) throw new AnimatInterfaceMMBSException("Bathymetry failed to load: " + mbs.ResultToTc(mbsResult));
            mbsResult = mbs.AddSpecies(species.SpeciesDefinitionFilePath);
            if (mbsRESULT.OK != mbsResult) throw new AnimatInterfaceMMBSException(string.Format("C3mbs::AddSpecies FATAL error {0} for species {1}", mbs.ResultToTc(mbsResult), species.SpeciesDefinitionFilePath));

            var bounds = new GeoArray(geoRect.NorthWest, geoRect.NorthEast, geoRect.SouthEast, geoRect.SouthWest, geoRect.NorthWest);
            var result = new Animat { ScenarioSpecies = species };
            var radius = Planet.wgs84_earthEquatorialRadiusMeters_D / 1000;
            var area = bounds.Area * radius * radius;
            //Debug.WriteLine("Area: {0}",area);
            var transformManyBlock = new TransformManyBlock<int, Geo<float>>(count =>
            {
                var geos = new List<Geo<float>>();
                for (var i = 0; i < count; i++)
                {
                    var location = bounds.RandomLocationWithinPerimeter();
                    var depth = bathymetry.Samples.GetNearestPointAsync(location).Result.Data;
                    mbsRESULT retval;
                    lock (mbs) retval = mbs.AddIndividualAnimat(0, new mbsPosition { latitude = location.Latitude, longitude = location.Longitude, depth = 0 });
                    if (mbsRESULT.OK == retval) geos.Add(new Geo<float>(location.Latitude, location.Longitude, (float)(depth * Random.NextDouble())));
                }
                return geos;
            }, new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                BoundedCapacity = -1,
                MaxDegreeOfParallelism = -1,
            });
            var bufferBlock = new BufferBlock<Geo<float>>();
            transformManyBlock.LinkTo(bufferBlock);
            var population = (int)Math.Round(area * species.PopulationDensity);
            result.TotalAnimats = population;
            const int blockSize = 100;
            while (population > 0)
            {
                transformManyBlock.Post(population > blockSize ? blockSize : population);
                population -= blockSize;
            }
            transformManyBlock.Complete();
            await transformManyBlock.Completion;
            //mbsResult = mbs.InitializeRun();
            //if (mbsRESULT.OK == mbsResult) while (mbsRUNSTATE.INITIALIZING == mbs.GetRunState()) Thread.Sleep(1);
            //else throw new AnimatInterfaceMMBSException("C3mbs::Initialize FATAL error " + mbs.ResultToTc(mbsResult));
            mbsResult = mbs.FinishRun();
            if (mbsRESULT.OK != mbsResult) throw new AnimatInterfaceMMBSException("C3mbs::FinishRun FATAL error " + mbs.ResultToTc(mbsResult));

            IList<Geo<float>> animatGeos;
            if (bufferBlock.TryReceiveAll(out animatGeos))
                result.Locations.AddRange(animatGeos);
            return result;
        }

        public override void Save(string filename)
        {
            using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None))
            using (var writer = new BinaryWriter(stream))
            {
                writer.Write(new[] { 'a', 'n', 'i', 'm' });
                //writer.Write(ScenarioSpecies.LatinName);
                writer.Write(Locations.Count);
                foreach (var sample in Locations)
                {
                    writer.Write(sample.Latitude);
                    writer.Write(sample.Longitude);
                    writer.Write(sample.Data);
                }
            }
        }
    }
}
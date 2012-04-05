using System;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;
using HRC;
using HRC.Navigation;

namespace ESME.Environment
{
    public class BottomLoss : EnvironmentDataSetBase
    {
        public EnvironmentData<BottomLossSample> Samples { get; private set; }

        public BottomLoss()
        {
            Samples = new EnvironmentData<BottomLossSample>();
        }

        public static Task<BottomLoss> LoadAsync(string filename)
        {
            return TaskEx.Run(() => Load(filename));
        }

        public static BottomLoss Load(string filename)
        {
            //return new BottomLoss { Samples = XmlSerializer<EnvironmentData<BottomLossSample>>.Load(filename, ReferencedTypes) };
            //var formatter = new BinaryFormatter();
            //using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
            //{
            //    return new BottomLoss { Samples = (EnvironmentData<BottomLossSample>)formatter.Deserialize(stream) };
            //}
            using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
            using (var reader = new BinaryReader(stream)) return Deserialize(reader);
        }

        public override void Save(string filename)
        {
            //var serializer = new XmlSerializer<EnvironmentData<BottomLossSample>> { Data = Samples };
            //serializer.Save(filename, ReferencedTypes);
            //var formatter = new BinaryFormatter();
            //using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None))
            //{
            //    formatter.Serialize(stream, Samples);
            //}
            Serialize(filename);
        }

        public void Serialize(string filename)
        {
            using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None))
            using (var writer = new BinaryWriter(stream))
                Serialize(writer);
        }

        public void Serialize(BinaryWriter writer)
        {
            writer.Write(Samples.Count);
            foreach (var item in Samples)
                item.Serialize(writer);
        }

        public static BottomLoss Deserialize(BinaryReader reader)
        {
            var result = new BottomLoss();
            var itemCount = reader.ReadInt32();
            for (var i = 0; i < itemCount; i++)
                result.Samples.Add(BottomLossSample.Deserialize(reader));
            return result;
        }
    }

    [Serializable]
    public class BottomLossSample : Geo<BottomLossData>, IComparer<BottomLossSample>
    {
        public BottomLossSample() { }

        public BottomLossSample(Geo location, BottomLossData sample) : base(location.Latitude, location.Longitude, sample) { }
        public BottomLossSample(double latitude, double longitude, BottomLossData sample) : base(latitude, longitude, sample) { }

        public new void Serialize(BinaryWriter writer)
        {
            base.Serialize(writer);
            Data.Serialize(writer);
        }

        public new static BottomLossSample Deserialize(BinaryReader reader)
        {
            return new BottomLossSample(Geo.Deserialize(reader), BottomLossData.Deserialize(reader));
        }

        public int Compare(BottomLossSample x, BottomLossSample y)
        {
            var compare = x.Latitude.CompareTo(y.Latitude); 
            return compare != 0 ? compare : x.Longitude.CompareTo(y.Longitude);
        }
    }

    // ReSharper disable InconsistentNaming
    [Serializable]
    public class BottomLossData : IComparer<BottomLossData>
    {
            public double CurveNumber { get; set; }
            public double RATIOD { get; set; }
            public double DLD { get; set; }
            public double RHOLD { get; set; }
            public double RHOSD { get; set; }
            public double GD { get; set; }
            public double BETAD { get; set; }
            public double FKZD { get; set; }
            public double FKZP { get; set; }
            public double BRFLD { get; set; }
            public double FEXP { get; set; }
            public double D2A { get; set; }
            public double ALF2A { get; set; }
            public double RHO2A { get; set; }
            public double SUBCRIT { get; set; }
            public double T2RH { get; set; }
            public double SEDTHK_M { get; set; }
            public double SEDTHK_S { get; set; }

        public void Serialize(BinaryWriter writer)
        {
            writer.Write(CurveNumber);
            writer.Write(RATIOD);
            writer.Write(DLD);
            writer.Write(RHOLD);
            writer.Write(RHOSD);
            writer.Write(GD);
            writer.Write(BETAD);
            writer.Write(FKZD);
            writer.Write(FKZP);
            writer.Write(BRFLD);
            writer.Write(FEXP);
            writer.Write(D2A);
            writer.Write(ALF2A);
            writer.Write(RHO2A);
            writer.Write(SUBCRIT);
            writer.Write(T2RH);
            writer.Write(SEDTHK_M);
            writer.Write(SEDTHK_S);
        }

        public static BottomLossData Deserialize(BinaryReader reader)
        {
            return new BottomLossData
            {
                CurveNumber = reader.ReadDouble(),
                RATIOD = reader.ReadDouble(),
                DLD = reader.ReadDouble(),
                RHOLD = reader.ReadDouble(),
                RHOSD = reader.ReadDouble(),
                GD = reader.ReadDouble(),
                BETAD = reader.ReadDouble(),
                FKZD = reader.ReadDouble(),
                FKZP = reader.ReadDouble(),
                BRFLD = reader.ReadDouble(),
                FEXP = reader.ReadDouble(),
                D2A = reader.ReadDouble(),
                ALF2A = reader.ReadDouble(),
                RHO2A = reader.ReadDouble(),
                SUBCRIT = reader.ReadDouble(),
                T2RH = reader.ReadDouble(),
                SEDTHK_M = reader.ReadDouble(),
                SEDTHK_S = reader.ReadDouble(),
            };
        }

        public int Compare(BottomLossData x, BottomLossData y)
        {
            var compare = x.CurveNumber.CompareTo(y.CurveNumber); if (compare != 0) return compare;
            compare = x.RATIOD.CompareTo(y.RATIOD); if (compare != 0) return compare;
            compare = x.DLD.CompareTo(y.DLD); if (compare != 0) return compare;
            compare = x.RHOLD.CompareTo(y.RHOLD); if (compare != 0) return compare;
            compare = x.RHOSD.CompareTo(y.RHOSD); if (compare != 0) return compare;
            compare = x.GD.CompareTo(y.GD); if (compare != 0) return compare;
            compare = x.BETAD.CompareTo(y.BETAD); if (compare != 0) return compare;

            compare = x.FKZD.CompareTo(y.FKZD); if (compare != 0) return compare;
            compare = x.FKZP.CompareTo(y.FKZP); if (compare != 0) return compare;
            compare = x.BRFLD.CompareTo(y.BRFLD); if (compare != 0) return compare;
            compare = x.FEXP.CompareTo(y.FEXP); if (compare != 0) return compare;
            compare = x.D2A.CompareTo(y.D2A); if (compare != 0) return compare;
            compare = x.ALF2A.CompareTo(y.ALF2A); if (compare != 0) return compare;

            compare = x.RHO2A.CompareTo(y.RHO2A); if (compare != 0) return compare;
            compare = x.SUBCRIT.CompareTo(y.SUBCRIT); if (compare != 0) return compare;
            compare = x.T2RH.CompareTo(y.T2RH); if (compare != 0) return compare;
            compare = x.SEDTHK_M.CompareTo(y.SEDTHK_M); if (compare != 0) return compare;
            return x.SEDTHK_S.CompareTo(y.SEDTHK_S);
        }
    }

    // ReSharper restore InconsistentNaming
}

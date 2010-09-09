using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace ESME.TransmissionLoss
{
    public class FieldData
    {
        public float Latitude_degrees { get; private set; }
        public float Longitude_degrees { get; private set; }
        public float SourceDepth_meters { get; private set; }
        public float VerticalBeamWidth_degrees { get; private set; }
        public float VerticalLookAngle_degrees { get; private set; }
        public float LowFrequency_Hz { get; private set; }
        public float HighFrequency_Hz { get; private set; }
        public float MaxCalculationDepth_meters { get; private set; }
        public long Radius_meters { get; private set; }
        public float[] Depths_meters { get; private set; }
        public float[] Ranges_meters { get; private set; }
        public RadialData[] Radials { get; private set; }
        public string Filename { get; set; }

        public static FieldData LoadHeader(string Filename)
        {
            return new FieldData(Filename, true);
        }
        
        public static FieldData Load(string Filename)
        {
            return new FieldData(Filename, false);
        }

        public FieldData(Field Field)
        {
            this.Latitude_degrees = Field.Latitude_degrees;
            this.Longitude_degrees = Field.Longitude_degrees;
            this.SourceDepth_meters = Field.SourceDepth_meters;
            this.VerticalBeamWidth_degrees = Field.VerticalBeamWidth_degrees;
            this.VerticalLookAngle_degrees = Field.VerticalLookAngle_degrees;
            this.LowFrequency_Hz = Field.LowFrequency_Hz;
            this.HighFrequency_Hz = Field.HighFrequency_Hz;
            this.MaxCalculationDepth_meters = Field.MaxCalculationDepth_meters;
            this.Radius_meters = Field.Radius_meters;
            this.Depths_meters = Field.Depths_meters;
            this.Ranges_meters = Field.Ranges_meters;
            this.Filename = Path.Combine(Field.DataDirectoryPath, Field.BinaryFileName);
        }

        public FieldData(string Filename, bool LoadHeadersOnly)
        {
            this.Filename = Filename;
            Load(LoadHeadersOnly);
        }

        public void Load(bool LoadHeadersOnly)
        {
            int i;
            bool eof = false;

            if (Filename == null)
                throw new FileNotFoundException("TransmissionLossFieldData: Specify a filename before calling Load()");
            using (BinaryReader stream = new BinaryReader(File.Open(this.Filename, FileMode.Open)))
            {
                if (stream.ReadUInt32() != _Magic)
                    throw new FileFormatException("Attempted to read invalid data into a TransmissionLossFieldData object");
                Latitude_degrees = stream.ReadSingle();
                Longitude_degrees = stream.ReadSingle();
                SourceDepth_meters = stream.ReadSingle();
                VerticalBeamWidth_degrees = stream.ReadSingle();
                VerticalLookAngle_degrees = stream.ReadSingle();
                LowFrequency_Hz = stream.ReadSingle();
                HighFrequency_Hz = stream.ReadSingle();
                MaxCalculationDepth_meters = stream.ReadSingle();
                Radius_meters = stream.ReadInt32();
                Depths_meters = new float[stream.ReadInt32()];
                for (i = 0; i < Depths_meters.Length; i++)
                    Depths_meters[i] = stream.ReadSingle();
                Ranges_meters = new float[stream.ReadInt32()];
                for (i = 0; i < Ranges_meters.Length; i++)
                    Ranges_meters[i] = stream.ReadSingle();
                mSaved = true;
                mRadials.Clear();
                try
                {
                    while (!eof)
                        mRadials.Add(new RadialData(stream, LoadHeadersOnly));
                }
                catch (EndOfStreamException)
                {
                    eof = true;
                }
                mRadials.Sort();
                Radials = mRadials.ToArray();
            }
        }

        public void AddRadial(RadialData Radial)
        {
            if (Radial.TransmissionLoss_dBSPL == null)
                throw new ApplicationException("TransmissionLossFieldData: Attempt to add a new radial that is not completely loaded into memory.  This operation is not supported.");
            mRadials.Add(Radial);
            mRadials.Sort();
            Radials = mRadials.ToArray();
        }

        public void Save(bool DiscardRadialDataAfterSave)
        {
            bool SaveSucceeded = false;
            if (Filename == null)
                throw new FileNotFoundException("TransmissionLossFieldData: Specify a filename before calling Save()");
            if (!Directory.Exists(Path.GetDirectoryName(this.Filename)))
                Directory.CreateDirectory(Path.GetDirectoryName(this.Filename));

            for (int retry = 0; retry < 10; retry++)
            {
                try
                {
                    using (BinaryWriter stream = new BinaryWriter(File.Open(this.Filename, FileMode.OpenOrCreate, FileAccess.Write)))
                    {
                        if (!mSaved)
                        {
                            stream.Write(_Magic);
                            stream.Write(Latitude_degrees);
                            stream.Write(Longitude_degrees);
                            stream.Write(SourceDepth_meters);
                            stream.Write(VerticalBeamWidth_degrees);
                            stream.Write(VerticalLookAngle_degrees);
                            stream.Write(LowFrequency_Hz);
                            stream.Write(HighFrequency_Hz);
                            stream.Write(MaxCalculationDepth_meters);
                            stream.Write(Radius_meters);
                            stream.Write(Depths_meters.Length);
                            foreach (float depth in Depths_meters)
                                stream.Write(depth);
                            stream.Write(Ranges_meters.Length);
                            foreach (float range in Ranges_meters)
                                stream.Write(range);
                            mSaved = true;
                        }
                        foreach (RadialData radial in mRadials)
                            radial.Save(stream, DiscardRadialDataAfterSave);
                        SaveSucceeded = true;
                        break;
                    }
                }
                catch (IOException)
                {
                    System.Threading.Thread.Sleep(1000);
                }
            }
            if (!SaveSucceeded)
                throw new IOException("FieldData: Could not save file.  Retry count exhausted.");
        }

        private const UInt32 _Magic = 0x99f84752;
        private List<RadialData> mRadials = new List<RadialData>();
        private bool mSaved = false;
    }
}

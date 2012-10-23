using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Runtime.InteropServices;

namespace MMMBSLib
{


    public class Utilities
    {
        public static FileStream Open(String sFileName)
        {
            return new FileStream(sFileName, FileMode.Open, FileAccess.Read, FileShare.Read);
        }

    }

    // Maybe get rid of all that are below here....
    public class CFSTestStructReader
    {
        // Member variables
        protected TestStruct m_data;
        protected string m_sFileName;
        private FileStream m_fs;

        private long m_lLength = -1;
        protected bool m_bOpen = false;
        private long m_lPosition = 0;
        private bool m_bUseCachedValuesForEOF = false;


        // Constructors
        public CFSTestStructReader(string sFileName)
        {
            m_sFileName = sFileName;
        }

        // Properties
        public TestStruct Data
        {
            get { return m_data; }
        }

        public bool EOF
        {
            get
            {
                if(m_bUseCachedValuesForEOF)
                    return (!m_bOpen || (m_lPosition >= m_lLength));
                else
                    return (!m_bOpen || (m_fs.Position >= m_fs.Length));
            }
        }

        public bool UseCachedValuesForEOF
        {
            get { return m_bUseCachedValuesForEOF; }
            set { m_bUseCachedValuesForEOF = value; }
        }


        // Class Methods
        public void Close()
        {
            m_bOpen = false;
            m_fs.Close();
            m_fs = null;
        }

        public void Open()
        {
            m_fs = new FileStream(m_sFileName, FileMode.Open, FileAccess.Read, FileShare.Read);
            m_lLength = m_fs.Length;
            m_lPosition = 0;
            m_bOpen = true;
        }

        public bool Read()
        {
            if(!EOF)
            {
                m_data = TestStruct.FromFileStream(m_fs);
                m_lPosition += TSSize.Size;
                return true;
            }
            else
            {
                return false;
            }
        }
    }



    /// <summary>
    /// Summary description for TestStruct.
    /// </summary>
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct TestStruct
    {
        public long longField;
        public byte byteField;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=16)]
        public byte[] byteArrayField;
        public float floatField;

        public static TestStruct FromFileStream(FileStream fs)
        {
            byte[] buff = new byte[TSSize.Size];//faster than [Marshal.SizeOf(typeof(TestStruct))];
            int amt = 0;
            while(amt < buff.Length)
                amt += fs.Read(buff, amt, buff.Length - amt);
            GCHandle handle = GCHandle.Alloc(buff, GCHandleType.Pinned);
            TestStruct s = (TestStruct)Marshal.PtrToStructure(handle.AddrOfPinnedObject(), typeof(TestStruct));
            handle.Free();
            return s;
        }
    }


    internal sealed class TSSize
    {
        public static int _size;

        static TSSize()
        {
            _size = Marshal.SizeOf(typeof(TestStruct));
        }

        public static int Size
        {
            get { return _size; }
        }
    }

    public class BinFileStruct
    {
        private static BinaryWriter m_bw = null;

        public static void Close()
        {
            if(m_bw == null)
                return;
            m_bw.Close();

            m_bw = null;
        }

        public static void WriteStruct(FileStream Fs, object Struct)
        {
            byte[] buffer;
            GCHandle h;

            try
            {
                // This function copys the structure data into a byte[]
                buffer = new byte[Marshal.SizeOf(Struct)]; // Set the buffer ot the correct size
                h = GCHandle.Alloc(buffer, GCHandleType.Pinned); //Allocate the buffer to memory and pin it so that GC cannot use the space (Disable GC)
                Marshal.StructureToPtr(Struct, h.AddrOfPinnedObject(), false); // copy the struct into int byte[] mem alloc 
                h.Free(); //Allow GC to do its job

                if(m_bw == null)
                    m_bw = new BinaryWriter(Fs);
                m_bw.Write(buffer);
            }
            catch(Exception ex)
            {
                throw ex;
            }

        }


        public static object ReadStruct(FileStream Fs, System.Type Type)
        {
            byte[] buffer = new byte[Marshal.SizeOf(Type)];
            object retObj = null;

            // Make sure the file stream is valid.
            if(Fs == null)
                return null;

            // Check for end of file.
            if(Fs.Position >= Fs.Length)
            {
                Fs.Close();
                GC.Collect();
            }

            try
            {
                Fs.Read(buffer, 0, buffer.Length);

                GCHandle handle = GCHandle.Alloc(buffer, GCHandleType.Pinned);
                retObj = (object)Marshal.PtrToStructure(handle.AddrOfPinnedObject(), Type);
                handle.Free();
                return retObj;
            }
            catch(Exception ex)
            {
                throw ex;
            }
        }
    }



    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct BEHAVIORDEF
    {
        // Member Variables
        public BEHAVIORTRANSITIONDEF behaviorTrans; // Behavior Transition Definition
        public DIRECTIONDEF direction; // Direction Definintion
        public RATEDEF travelRate; // Travel Definition
        public DIVEDEF dive; // Dive Definition.

        //public StreamWriter fileWriter;
        //public FileStream output;

        // Constructors


        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            behaviorTrans.Clear();
            direction.Clear();
            travelRate.Clear();
            dive.Clear();
        }

        public void SaveToBinFile(FileStream Fs)
        {
            //BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            //this = ()BinFileStruct.ReadStruct(Fs, this.GetType());
        }

        public void SetNormalDefaultBehavior()
        {

        }
    }

    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct BEHAVIORTRANSITIONDEF
    {
        public BEHAVIORMTXMDL vm;
        private System.Type _type;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=12)]
        private byte[] _reserved;

        // Structure Properties
        public System.Type type
        {
            get { return _type; }
            set
            {
                if(value == typeof(BEHAVIORMTXMDL) || value == typeof(NOTDEFINED))
                    _type = value;
            }
        }

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            vm.Clear();
            _type = typeof(NOTDEFINED);
        }
        public void SaveToBinFile(FileStream Fs)
        {
            //BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            //this = ()BinFileStruct.ReadStruct(Fs, this.GetType());
        }
    }

    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct DIRECTIONDEF
    {
        public DIRECTIONRW rw; // Random Walk 
        public DIRECTIONCRW crw; // Correlated Random Walk
        public DIRECTIONCRWDB crwdb; // Correlated Random Walk with Directional Bias
        public DIRECTIONMTXMDL vm; // Vector Model
        private System.Type _type;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=12)]
        private byte[] _reserved;

        // Structure Properties
        public System.Type type
        {
            get { return _type; }
            set
            {
                if(value == typeof(DIRECTIONRW) || value == typeof(DIRECTIONCRW) ||
                    value == typeof(DIRECTIONCRWDB) || value == typeof(DIRECTIONMTXMDL) ||
                    value == typeof(NOTDEFINED))
                    _type = value;
            }
        }


        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            rw.Clear();
            crw.Clear();
            crwdb.Clear();
            vm.Clear();
            _type = typeof(NOTDEFINED);
        }

        public void SaveToBinFile(FileStream Fs)
        {
            //BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            //this = ()BinFileStruct.ReadStruct(Fs, this.GetType());
        }
    }


    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct RATEDEF
    {
        public RATEGAUSS gauss; // Gaussian
        public RATERANDOM random; // Random
        public RATEMTXMDL vm; // Vector Model
        private System.Type _type;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=12)]
        private byte[] _reserved;

        // Structure Properties
        public System.Type type
        {
            get { return _type; }
            set
            {
                if(value == typeof(RATEGAUSS) || value == typeof(RATERANDOM) ||
                    value == typeof(RATEMTXMDL) || value == typeof(NOTDEFINED))
                    _type = value;
            }
        }

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            gauss.Clear();
            random.Clear();
            vm.Clear();
            _type = typeof(NOTDEFINED);
        }

        public void SaveToBinFile(FileStream Fs)
        {
            //BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            //this = ()BinFileStruct.ReadStruct(Fs, this.GetType());
        }
    }

    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct DIVEDEF
    {
        public RATEDEF ascentRate; // Ascent Rate
        public RATEDEF descentRate; // Descent Rate
        public DEPTHDEF depth;   // Depth
        public FLATBOTTOMDIVEDEF flatBottomDive; // Flat bottom Diving
        public REVERSALSDEF reversals; // Reversals
        public SURFACEINTERVALDEF surfaceInterval; // Surface Interval

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            ascentRate.Clear();
            descentRate.Clear();
            depth.Clear();
            flatBottomDive.Clear();
            reversals.Clear();
            surfaceInterval.Clear();
        }

        public void SaveToBinFile(FileStream Fs)
        {
            //BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            //this = ()BinFileStruct.ReadStruct(Fs, this.GetType());
        }
    }

    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct DEPTHDEF
    {
        public DEPTHGAUSS gauss; // Gaussian
        public DEPTHRANDOM random; // Random
        public DEPTHMTXMDL vm; // Vector Model
        private System.Type _type;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=12)]
        private byte[] _reserved;

        // Structure Properties
        public System.Type type
        {
            get { return _type; }
            set
            {
                if(value == typeof(DEPTHGAUSS) || value == typeof(DEPTHRANDOM) ||
                    value == typeof(DEPTHMTXMDL) || value == typeof(NOTDEFINED))
                    _type = value;
            }
        }

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            gauss.Clear();
            random.Clear();
            vm.Clear();
            _type = typeof(NOTDEFINED);
        }

        public void SaveToBinFile(FileStream Fs)
        {
            //BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            //this = ()BinFileStruct.ReadStruct(Fs, this.GetType());
        }
    }
    
    //----------------------------------------------------------------------------------//
    // Dive Submodels
    //---------------//
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct FLATBOTTOMDIVEDEF
    {
        public FLATBOTTOMDIVEBOOLEAN boolMdl;
        private System.Type _type;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=12)]
        private byte[] _reserved;
        // Structure Properties
        public System.Type type
        {
            get { return _type; }
            set
            {
                if(value == typeof(FLATBOTTOMDIVEBOOLEAN)|| value == typeof(NOTDEFINED))
                    _type = value;
            }
        }

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            boolMdl.Clear();
            _type = typeof(NOTDEFINED);
        }

        public void SaveToBinFile(FileStream Fs)
        {
            //BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            //this = ()BinFileStruct.ReadStruct(Fs, this.GetType());
        }

    }

    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct REVERSALSDEF
    {
        // Structure Variables
        public REVERSALGAUSS gauss; // Gaussian
        public REVERSALRANDOM random; // Random
        public REVERSALMTXMDL vm; // Vector Model
        private System.Type _type;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=12)]
        private byte[] _reserved;
        
        // Structure Properties
        public System.Type type
        {
            get { return _type; }
            set
            {
                if(value == typeof(REVERSALGAUSS) || value == typeof(REVERSALRANDOM) ||
                    value == typeof(REVERSALMTXMDL) || value == typeof(NOTDEFINED))
                    _type = value;
            }
        }

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            gauss.Clear();
            random.Clear();
            vm.Clear();
            _type = typeof(NOTDEFINED);
        }

        public void SaveToBinFile(FileStream Fs)
        {
   //         BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
     //       this = ()BinFileStruct.ReadStruct(Fs, this.GetType());
        }
    }

    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct SURFACEINTERVALDEF
    {
        public SURFACEINTERVALGAUSS gauss; // Gaussian
        public SURFACEINTERVALMTXMDL vm; // Vector Model
        private Type _type;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=12)]
        private byte[] _reserved;

        // Structure Properties
        public System.Type type
        {
            get { return _type; }
            set
            {
                if(value == typeof(SURFACEINTERVALGAUSS) ||
                    value == typeof(SURFACEINTERVALMTXMDL) || value == typeof(NOTDEFINED))
                    _type = value;
            }
        }

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            gauss.Clear();
            vm.Clear();
            _type = typeof(NOTDEFINED);
        }

        public void SaveToBinFile(FileStream Fs)
        {
            _reserved = new byte[12];
            gauss.SaveToBinFile(Fs);
            vm.SaveToBinFile(Fs);
           //_type.
            //BinFileStruct.WriteStruct(Fs, _type);
            BinFileStruct.WriteStruct(Fs, _reserved);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            gauss.LoadFromBinFile(Fs);
            vm.LoadFromBinFile(Fs);
            //_type = (System.Type)BinFileStruct.ReadStruct(Fs, _type.GetType());
            _reserved = (byte[])BinFileStruct.ReadStruct(Fs, _reserved.GetType());
        }
    }

    //----------------------------------------------------------------------------------//
    // Sub Definitions
    //----------------//
    // Behavior Sub Definitions
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct BEHAVIORMTXMDL
    {
        public MATRIX behavior;
        public MATRIX initial;
        public VECTOR terminate;

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            behavior.Clear();
            initial.Clear();
            terminate.Clear();
        }

        public void SaveToBinFile(FileStream Fs)
        {
            behavior.SaveToBinFile(Fs);
            initial.SaveToBinFile(Fs);
            terminate.SaveToBinFile(Fs);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            behavior.LoadFromBinFile(Fs);
            initial.LoadFromBinFile(Fs);
            terminate.LoadFromBinFile(Fs);
        }
    }

    // Direction Sub Definitions
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct DIRECTIONRW
    {
        public double coeff;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=8)]
        private byte[] _reserved;

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            coeff = 0;
        }

        public void SaveToBinFile(FileStream Fs)
        {
            BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            this = (DIRECTIONRW)BinFileStruct.ReadStruct(Fs, this.GetType());
        }
    }

    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct DIRECTIONCRW
    {
        public double pert; // Perturbation
        public double coeff;

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            pert = 0;
            coeff = 0;
        }

        public void SaveToBinFile(FileStream Fs)
        {
            BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            this = (DIRECTIONCRW)BinFileStruct.ReadStruct(Fs, this.GetType());
        }
    }

    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct DIRECTIONCRWDB
    {
        public double pert; // used as standard deviation in noise function.
        public double directionOfBias; // current heading?
        public double bias;
        public double arcStep;
        public double coeff;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=8)]
        private byte[] _reserved;

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            pert = 0;
            directionOfBias = 0;
            bias = 0;
            arcStep = 0;
            coeff = 0;
        }

        public void SaveToBinFile(FileStream Fs)
        {
            BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            this = (DIRECTIONCRWDB)BinFileStruct.ReadStruct(Fs, this.GetType());
        }
    }
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct DIRECTIONMTXMDL
    {
        public MATRIX direction;
        public MATRIX bias;
        public VECTOR terminate;

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            direction.Clear();
            bias.Clear();
            terminate.Clear();
        }

        public void SaveToBinFile(FileStream Fs)
        {
            direction.SaveToBinFile(Fs);
            bias.SaveToBinFile(Fs);
            terminate.SaveToBinFile(Fs);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            direction.LoadFromBinFile(Fs);
            bias.LoadFromBinFile(Fs);
            terminate.LoadFromBinFile(Fs);
        }
    }

    // Rate Sub Definitions
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct RATEGAUSS
    {
        public double mean;
        public double std;
        public double coeff;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=8)]
        private byte[] _reserved;

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            mean = 0;
            std = 0;
            coeff = 0;
        }
        public void SaveToBinFile(FileStream Fs)
        {
            BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            this = (RATEGAUSS)BinFileStruct.ReadStruct(Fs, this.GetType());
        }

    }
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct RATERANDOM
    {
        public double max;
        public double min;
        public double coeff;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=8)]
        private byte[] _reserved;


        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            max = 0;
            min = 0;
            coeff = 0;
        }
        public void SaveToBinFile(FileStream Fs)
        {
            BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            this = (RATERANDOM)BinFileStruct.ReadStruct(Fs, this.GetType());
        }



    }
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct RATEMTXMDL
    {
        public MATRIX rate;
        public ELEMENT step;
        public VECTOR terminate;


        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            rate.Clear();
            step.Clear();
            terminate.Clear();
        }

        public void SaveToBinFile(FileStream Fs)
        {
            rate.SaveToBinFile(Fs);
            step.SaveToBinFile(Fs);
            terminate.SaveToBinFile(Fs);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            rate.LoadFromBinFile(Fs);
            step.LoadFromBinFile(Fs);
            terminate.LoadFromBinFile(Fs);
        }

    }

    // Depth Sub Definitions
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct DEPTHGAUSS
    {
        public double mean;
        public double std;

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            mean = 0;
            std = 0;
        }
        public void SaveToBinFile(FileStream Fs)
        {
            BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            this = (DEPTHGAUSS)BinFileStruct.ReadStruct(Fs, this.GetType());
        }
    }
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct DEPTHRANDOM
    {
        public double max;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=8)]
        private byte[] _reserved;

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            max = 0;
        }
        public void SaveToBinFile(FileStream Fs)
        {
            BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            this = (DEPTHRANDOM)BinFileStruct.ReadStruct(Fs, this.GetType());
        }

    }
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct DEPTHMTXMDL
    {
        public MATRIX vector;
        public ELEMENT step;

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            vector.Clear();
            step.Clear();
        }
        public void SaveToBinFile(FileStream Fs)
        {
            vector.SaveToBinFile(Fs);
            step.SaveToBinFile(Fs);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            vector.LoadFromBinFile(Fs);
            step.LoadFromBinFile(Fs);
        }
    }

    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct FLATBOTTOMDIVEBOOLEAN
    {
        public bool engages;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=12)]
        private byte[] _reserved;

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            engages = false;
        }
        public void SaveToBinFile(FileStream Fs)
        {
            BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            this = (FLATBOTTOMDIVEBOOLEAN)BinFileStruct.ReadStruct(Fs, this.GetType());
        }
    }

    // Reversal Sub Definitions
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct REVERSALGAUSS
    {
        public int countMean;
        public int countStd;
        public double prob;
        public double timeReversalMean;
        public double timeReversalStd;

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            countMean = 0;
            countStd = 0;
            prob = 0;
            timeReversalMean = 0;
            timeReversalStd = 0;
        }
        public void SaveToBinFile(FileStream Fs)
        {
            BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            this = (REVERSALGAUSS)BinFileStruct.ReadStruct(Fs, this.GetType());
        }

    }
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct REVERSALRANDOM
    {
        public int countMin;
        public int countMax;
        public double prob;
        public double timeReversalMean;
        public double timeReversalStd;

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            countMin = 0;
            countMax = 0;
            prob = 0;
            timeReversalMean = 0;
            timeReversalStd = 0;
        }

        public void SaveToBinFile(FileStream Fs)
        {
            BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            this = (REVERSALRANDOM)BinFileStruct.ReadStruct(Fs, this.GetType());
        }
    }
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct REVERSALMTXMDL
    {
        public MATRIX reversalMatrix;
        public VECTOR probVector;
        public MATRIX timeReverseMatrix;
        public ELEMENT timeReversedStep;

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            reversalMatrix.Clear();
            probVector.Clear();
            timeReverseMatrix.Clear();
            timeReversedStep.Clear();
        }
        public void SaveToBinFile(FileStream Fs)
        {
            reversalMatrix.SaveToBinFile(Fs);
            probVector.SaveToBinFile(Fs);
            timeReverseMatrix.SaveToBinFile(Fs);
            timeReversedStep.SaveToBinFile(Fs);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            reversalMatrix.LoadFromBinFile(Fs);
            probVector.LoadFromBinFile(Fs);
            timeReverseMatrix.LoadFromBinFile(Fs);
            timeReversedStep.LoadFromBinFile(Fs);
        }
    }

    // Surface Interval Sub Definitions
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct SURFACEINTERVALGAUSS
    {
        public double mean;
        public double std;

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            mean = 0;
            std = 0;
        }
        public void SaveToBinFile(FileStream Fs)
        {
            BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            this = (SURFACEINTERVALGAUSS)BinFileStruct.ReadStruct(Fs, this.GetType());
        }
    }

    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct SURFACEINTERVALMTXMDL
    {
        public MATRIX vector;
        public ELEMENT step;


        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            vector.Clear();
            step.Clear();
        }

        public void SaveToBinFile(FileStream Fs)
        {
            vector.SaveToBinFile(Fs);
            step.SaveToBinFile(Fs);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            vector.LoadFromBinFile(Fs);
            step.LoadFromBinFile(Fs);
        }
    }

    public struct NOTDEFINED
    {
        // empty structure
    }

    //----------------------------------------------------------------------------------//
    // Matrix, Vector, Element definitions
    //------------------------------------//
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct MATRIX
    {
        public int nNumRows;
        public int nNumCols;
        public double a; // Elements in the Matrix 

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            nNumRows = 0;
            nNumCols = 0;
            a = 0;
        }
        public void SaveToBinFile(FileStream Fs)
        {
            BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            this = (MATRIX)BinFileStruct.ReadStruct(Fs, this.GetType());
        }

    }
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct VECTOR
    {
        public int nNumRows;
        public int nNumCols;
        public double a; // Elements in the vector 

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            nNumRows = 0;
            nNumCols = 0;
            a = 0;
        }

        public void SaveToBinFile(FileStream Fs)
        {
            BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            this = (VECTOR)BinFileStruct.ReadStruct(Fs, this.GetType());
        }

    }
    [StructLayout(LayoutKind.Sequential, Pack=1)]
    public struct ELEMENT
    {
        public int nNumRows;
        public int nNumCols;
        public double a; // Elements in the Element 

        //------------------//
        // Structure Methods
        //------------------//
        public void Clear()
        {
            nNumRows = 1;
            nNumCols = 1;
            a = 0;
        }
        public void SaveToBinFile(FileStream Fs)
        {
            BinFileStruct.WriteStruct(Fs, this);
        }
        public void LoadFromBinFile(FileStream Fs)
        {
            this = (ELEMENT) BinFileStruct.ReadStruct(Fs, this.GetType());
        }


#if false
        public void BinFileWrite(FileStream fs)
        {
            byte[] buff = new byte[Marshal.SizeOf(typeof(ELEMENT))];
            GCHandle handle = GCHandle.Alloc(this, GCHandleType.Pinned);
            //Marshal.StructureToPtr(handle, buff, false);
            //Marshal.StructureToPtr(handle, buff, false);
            fs.Write(buff, 0, buff.Length);
            handle.Free();
        }
        public void BinFileRead(FileStream fs)
        {
            byte[] buff = new byte[Marshal.SizeOf(typeof(ELEMENT))];
            int amt = 0;
            while(amt < buff.Length)
                amt += fs.Read(buff, amt, buff.Length - amt);
            GCHandle handle = GCHandle.Alloc(buff, GCHandleType.Pinned);
            this = (ELEMENT)Marshal.PtrToStructure(handle.AddrOfPinnedObject(), typeof(ELEMENT));
            handle.Free();
        }
#endif
    }


}

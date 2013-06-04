using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using MMMBSLib;
using System.Runtime.InteropServices;
//using mbs;


namespace ConsoleApplication
{
    class Program
    {
        static void Main(string[] args)
        {

#if false
            CReversals rev = null;
            CReversalsGaussn revG = new CReversalsGaussn();
            CReversalsRandom revR = new CReversalsRandom();
            CReversalsMatrix revM = new CReversalsMatrix();


            rev = revG;
            t = rev.GetType();

            rev = revM;
            t = rev.GetType();
            t = rev.GetType();


            //TestStruct ts;
            //FileStream fs;
            int size;
#endif
#if false
            SURFACEINTERVALDEF e;
            FileStream fs = null;

            fs = new FileStream(
                ".\\dog.bin", 
                System.IO.FileMode.Create,
                System.IO.FileAccess.Write, 
                System.IO.FileShare.None);

            e = new SURFACEINTERVALDEF();
            e.Clear();
            e.gauss.mean = 45;
            e.gauss.std = 5;
            e.SaveToBinFile(fs);
            //BinFileStruct.WriteStruct(fs, e);
            fs.Close();
            GC.Collect();

            fs = new FileStream(
                ".\\dog.bin",
                System.IO.FileMode.Open,
                System.IO.FileAccess.Read,
                System.IO.FileShare.None);
            e.LoadFromBinFile(fs);
            //e = (ELEMENT)BinFileStruct.ReadStruct(fs, e.GetType());
            fs.Close();
            GC.Collect();
#endif


            // Keep this for future reference
#if false
            //SURFACEINTERVALDEF sid;

            //bool enabled = sid.state.type;
            int size;
            size = Marshal.SizeOf(typeof(BEHAVIORDEF));
            Console.WriteLine("Sizeof BEHAVIORDEF = {0} mod 16 = {1}", size, size % 16);

            size = Marshal.SizeOf(typeof(NOTDEFINED));
            Console.WriteLine("Sizeof NOTDEFINED = {0} mod 16 = {1}", size, size % 16);

            size = Marshal.SizeOf(typeof(BEHAVIORTRANSITIONDEF));
            Console.WriteLine("Sizeof BEHAVIORTRANSITIONDEF = {0} mod 16 = {1}", size, size%16);

            size = Marshal.SizeOf(typeof(DIRECTIONDEF));
            Console.WriteLine("Sizeof DIRECTIONDEF = {0} mod 16 = {1}", size, size%16);

            size = Marshal.SizeOf(typeof(DIVEDEF));
            Console.WriteLine("Sizeof DIVEDEF = {0} mod 16 = {1}", size, size%16);

            size = Marshal.SizeOf(typeof(DEPTHDEF));
            Console.WriteLine("Sizeof DEPTHDEF = {0} mod 16 = {1}", size, size%16);


            size = Marshal.SizeOf(typeof(FLATBOTTOMDIVEDEF));
            Console.WriteLine("Sizeof FLATBOTTOMDIVEDEF = {0} mod 16 = {1}", size, size%16);

            size = Marshal.SizeOf(typeof(SURFACEINTERVALGAUSS));
            Console.WriteLine("Sizeof SURFACEINTERVALGAUSS = {0} mod 16 = {1}", size, size%16);

            size = Marshal.SizeOf(typeof(SURFACEINTERVALMTXMDL));
            Console.WriteLine("Sizeof SURFACEINTERVALMTXMDL = {0} mod 16 = {1}", size, size%16);

            Type doggg = size.GetType();
            size = Marshal.SizeOf(doggg);
            Console.WriteLine("Sizeof System.Type = {0} mod 16 = {1}", size, size%16);


            size = Marshal.SizeOf(typeof(SURFACEINTERVALDEF));
            Console.WriteLine("Sizeof SURFACEINTERVALDEF = {0} mod 16 = {1}", size, size%16);
#endif

#if false
            CFSTestStructReader reader;
            reader = new CFSTestStructReader("D:\\cow.txt");
            reader.Open();
            reader.Read();
#endif
#if false
            FileStream fs = File.Open("D:\\cow.txt", FileMode.Open);
            for (i = 0; i < 4; i++)
            {
                MMMBSLib.Utilities.ReadWholeArray(fs, input);
                floatDog = (float)input;
            }
            fs.Close();
#endif
        }
    }
}

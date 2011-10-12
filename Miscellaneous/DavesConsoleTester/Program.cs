using System;

namespace DavesConsoleTester
{
    class Program
    {
        [STAThread]
        public static int Main(string[] args)
        {
            foreach (var arg in args)
            {
                switch (arg)
                {
                    default:
                        Usage();
                        return -1;
                }
            }
        }

        public static void Usage()
        {
            Console.WriteLine("Usage: CreateManyOverlays -name baseName -lat latitude -lon longitude ");
        }
    }
}

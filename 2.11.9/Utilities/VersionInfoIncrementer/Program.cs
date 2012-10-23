using System;
using System.IO;
using System.Reflection;

namespace VersionInfoIncrementer
{
    class Program
    {
        static int Main(string[] args)
        {
            //Console.Write("VersionInfoIncrementer command line: ");
            //foreach (var arg in args) Console.Write(arg + " ");
            //Console.WriteLine();
            if (args.Length != 1 || !File.Exists(args[0]))
            {
                Usage();
                return 1;
            }
            var lines = File.ReadAllLines(args[0]);
            var foundVersion = false;
            for (var lineIndex = 0; lineIndex < lines.Length; lineIndex++)
            {
                if (lines[lineIndex].StartsWith("//")) continue;
                if (foundVersion)
                {
                    Console.WriteLine("Error: Multiple version numbers found");
                    Usage();
                    return 1;
                }
                var curVersion = new Version(lines[lineIndex]);
                if (curVersion.Major < 0 || curVersion.Minor < 0 || curVersion.Build < 0)
                {
                    Console.WriteLine("Error: Major, Minor and Build version numbers must be defined");
                    Usage();
                    return 1;
                }
                var newVersion = new Version(curVersion.Major, curVersion.Minor, curVersion.Build + 1);
                lines[lineIndex] = newVersion.ToString();
                Console.Write(newVersion.ToString());
                foundVersion = true;
            }
            File.WriteAllLines(args[0], lines);
            return 0;
        }

        static void Usage()
        {
            Console.WriteLine("Usage: {0} <versionInfoFilename>", Path.GetFileNameWithoutExtension(Assembly.GetExecutingAssembly().Location));
        }
    }
}

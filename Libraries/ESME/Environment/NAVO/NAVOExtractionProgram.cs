using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;

namespace ESME.Environment.NAVO
{
    public static class NAVOExtractionProgram
    {
        public static string Execute(string extractionProgramPath, string commandArgs, string workingDirectory)
        {
            if (!File.Exists(extractionProgramPath)) throw new FileNotFoundException(string.Format("Could not locate specifed extraction program {0}", extractionProgramPath));

            var process = new Process
            {
                StartInfo = new ProcessStartInfo(extractionProgramPath)
                {
                    CreateNoWindow = true,
                    UseShellExecute = false,
                    RedirectStandardInput = false,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    Arguments = commandArgs,
                    WorkingDirectory = workingDirectory,
                },
            };
            process.Start();
            if (process.HasExited) return process.StandardOutput.ReadToEnd();

            process.PriorityClass = ProcessPriorityClass.BelowNormal;
            process.WaitForExit();
            return process.StandardOutput.ReadToEnd();
        }
    }
}
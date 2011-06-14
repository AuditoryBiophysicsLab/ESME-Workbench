using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;

namespace ESME.Environment.NAVO
{
    public static class NAVOExtractionProgram
    {
        public static string Execute(string extractionProgramPath, string commandArgs, string workingDirectory, IList<string> requiredSupportFiles)
        {
            if (!File.Exists(extractionProgramPath)) throw new FileNotFoundException(string.Format("Could not locate specifed extraction program {0}", extractionProgramPath));
            foreach (var supportFile in requiredSupportFiles.Where(supportFile => !File.Exists(supportFile))) 
                throw new FileNotFoundException(string.Format("Could not locate required support file {0} for extraction program {1}", supportFile, extractionProgramPath));
            return Execute(extractionProgramPath, commandArgs, workingDirectory);
        }

        public static string Execute(string extractionProgramPath, string commandArgs, string workingDirectory)
        {
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
            process.PriorityClass = ProcessPriorityClass.BelowNormal;
            var output = new StringBuilder();
            while (!process.HasExited)
            {
                output.Append(process.StandardOutput.ReadToEnd());
                Thread.Sleep(100);
            }
            return output.ToString();
        }
    }
}
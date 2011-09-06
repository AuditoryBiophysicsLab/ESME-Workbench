using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace ESME.Environment.NAVO
{
    public static class NAVOExtractionProgram
    {
        public async static Task<string> Execute(string extractionProgramPath, string commandArgs, string workingDirectory, IEnumerable<string> requiredSupportFiles)
        {
            if (!File.Exists(extractionProgramPath)) throw new FileNotFoundException(string.Format("Could not locate specifed extraction program {0}", extractionProgramPath));
            foreach (var supportFile in requiredSupportFiles.Where(supportFile => !File.Exists(supportFile))) 
                throw new FileNotFoundException(string.Format("Could not locate required support file {0} for extraction program {1}", supportFile, extractionProgramPath));
            return await ExecuteAsync(extractionProgramPath, commandArgs, workingDirectory);
        }

        public async static Task<string> ExecuteAsync(string extractionProgramPath, string commandArgs, string workingDirectory)
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
            if (process.HasExited) return process.StandardOutput.ReadToEnd();

            process.PriorityClass = ProcessPriorityClass.BelowNormal;
            while (!process.HasExited)
                TaskEx.Delay(100, CancellationToken.None).ConfigureAwait(false);
            return process.StandardOutput.ReadToEnd();
        }
    }
}
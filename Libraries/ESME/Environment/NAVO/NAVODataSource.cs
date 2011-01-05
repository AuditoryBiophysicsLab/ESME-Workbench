using System.Diagnostics;
using System.Text;
using System.Threading;

namespace ESME.Environment.NAVO
{
    public abstract class NAVODataSource
    {
        public string ExtractionProgramPath { get; set; }
        public string DatabasePath { get; set; } //change back to protected set?
        public string CommandArgs { get; protected set; }
        public Environment2DData ExtractedArea { get; protected set; }
        public string WorkingDirectory { get; set; }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="filename">output file name (in appropriate native-to-source format)</param>
        /// <param name="north"></param>
        /// <param name="south"></param>
        /// <param name="east"></param>
        /// <param name="west"></param>
        public abstract void ExtractArea(string filename, double north, double south, double east, double west);

        public abstract bool ValidateDataSource();

        protected string Execute()
        {
            var process = new Process
                          {
                              StartInfo = new ProcessStartInfo(ExtractionProgramPath)
                                          {
                                              CreateNoWindow = false,
                                              UseShellExecute = false,
                                              RedirectStandardInput = false,
                                              RedirectStandardOutput = true,
                                              RedirectStandardError = true,
                                              Arguments = CommandArgs,
                                              WorkingDirectory = WorkingDirectory,
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
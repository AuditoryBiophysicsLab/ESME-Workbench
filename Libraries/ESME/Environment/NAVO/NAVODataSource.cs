using System;
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
        public EnvironmentData ExtractedArea { get; protected set; }
        public string WorkingDirectory { get; set; }
        public float GridSpacing { get; set; }
        public string TimePeriod { get; set; }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="packet"></param>
        //public abstract void ExtractArea(string filename, double north, double south, double east, double west);
        public abstract void ExtractArea(NAVOExtractionPacket packet);

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

    public class NAVOExtractionPacket
    {
        public string Filename;
        public double North;
        public double South;
        public double East;
        public double West;

        public NAVOExtractionPacket()
        {
            //if (Filename == null) throw new ApplicationException("NAVO Data Source: a file name must be specified");
           // if ((West >= East) || (South >= North)) throw new ApplicationException("GIS parameters are out of range. West must be less than east, and south must be less than north.");
        }
    }
}

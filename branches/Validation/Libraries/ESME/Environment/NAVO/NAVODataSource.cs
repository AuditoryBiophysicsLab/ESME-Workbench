using System.Diagnostics;
using System.Text;
using System.Threading;
using Cinch;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
    public abstract class NAVODataSource : ViewModelBase
    {
        public string ExtractionProgramPath { get; set; }
        public string DatabasePath { get; set; } //change back to protected set?
        public string CommandArgs { get; protected set; }
        public Environment2DData<EarthCoordinate<float>> ExtractedArea { get; protected set; }
        public string WorkingDirectory { get; set; }
        public float GridSpacing { get; set; }  //in degrees.
        public NAVOTimePeriod TimePeriod { get; set; }
        public int StartMonth { get; set; }
        public int EndMonth { get; set; }
        public int MonthsDuration { get; set; }
        public string OutputFilename { get; set; }

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
                                              CreateNoWindow = true,
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

    public static class NAVOExtractionProgram
    {
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

    public class NAVOExtractionPacket
    {
        public string Filename { get; set; }
        public double North { get; set; }
        public double South { get; set; }
        public double East { get; set; }
        public double West { get; set; }
        public NAVOTimePeriod StartTime { get; set; }
        public NAVOTimePeriod EndTime { get; set; }
        public NAVOTimePeriod TimePeriod { get; set; }
    }

    public enum NAVOTimePeriod
    {
        January = 1,
        February,
        March,
        April,
        May,
        June,
        July,
        August,
        September,
        October,
        November,
        December,
        Spring,
        Summer,
        Fall,
        Winter,
        Cold,
        Warm,
    }
}
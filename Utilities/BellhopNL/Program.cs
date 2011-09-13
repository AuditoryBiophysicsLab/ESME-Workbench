using System;
using System.Diagnostics;
using System.IO;
using System.IO.Pipes;
using System.Reflection;

namespace BellhopNL
{
    class Program
    {
        static void Main(string[] args)
        {
            var arrivalsFile = "";
            var envFile = "";
            var chargeDepth = double.MinValue;
            var chargeMass = double.MinValue;
            var outputRate = double.MinValue;
            var outputDuration = double.MinValue;
            var modelType = BellhopNLWrapper.ModelType.error;
            try
            {
                if (args.Length < 12) throw new FormatException("a required parameter is missing.");
                for (var i = 0; i < args.Length; i++)
                {
                    switch (args[i])
                    {
                        case "-envFile":
                            envFile = args[++i];
                            if (string.IsNullOrEmpty(envFile) || !File.Exists(envFile) || Path.GetExtension(envFile) != ".env") throw new FormatException("the specified environment file does not exist.");
                            break;
                        case "-chargeDepth":
                            if (!double.TryParse(args[++i], out chargeDepth) || chargeDepth < 0) throw new FormatException("The specified charge depth is invalid");
                            break;
                        case "-chargeMass":
                            if (!double.TryParse(args[++i], out chargeMass) || chargeMass <= 0) throw new FormatException("The specified charge mass is invalid");
                            break;
                        case "-outputRate":
                            if (!double.TryParse(args[++i], out outputRate) || outputRate <= 0) throw new FormatException("The specified output rate is invalid");
                            break;
                        case "-outputDuration":
                            if (!double.TryParse(args[++i], out outputDuration) || outputDuration <= 0) throw new FormatException("The specified output duration is invalid");
                            break;
                        case "-modelType":
                            if (!Enum.TryParse(args[++i], true, out modelType) || modelType == BellhopNLWrapper.ModelType.error) throw new FormatException("The specified model type is invalid");
                            break;
                        default:
                            Useage();
                            return;
                    }
                }
            }
            catch (FormatException exception)
            {
                Console.WriteLine("Input parameter was incorrect: " + exception.Message);
            }
#if  false

            //todo: given a .env file, can we parse out chargeDepth,chargeMass,outputRate, &. ?  might only need envFile and modelType. 
            // run bellhop with env file. 
            var workingDirectory = "";
            var bellhopExecutable = Path.Combine(Path.GetDirectoryName(Assembly.GetCallingAssembly().Location), "bellhop.exe"); //incorrect, almost certainly.
            var info = new ProcessStartInfo(bellhopExecutable, "bellhop")
            {
                WorkingDirectory = workingDirectory,
                CreateNoWindow = true,
                WindowStyle = ProcessWindowStyle.Hidden,
                UseShellExecute = false,
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true
            };
            var process = Process.Start(bellhopExecutable, "bellhop"); 

            while (process != null && !process.HasExited)
            {
                arrivalsFile += process.StandardOutput; //?
            }
#endif
            //give NLWrapper the arrivals file. 
            var result =BellhopNLWrapper.Run(arrivalsFile, chargeDepth, chargeMass, outputRate, outputDuration, modelType).Waveforms;

            //todo: make a refms-like file from this data.
        }

        static void Useage() { throw new NotImplementedException(); }
    }
}
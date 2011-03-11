using System.Collections.Generic;
using System.IO;
using System.Windows.Threading;
using ESME.Data;
using ESME.Environment;
using ESME.NEMO;

namespace ESME.TransmissionLoss
{
    public class TransmissionLossFileManager
    {
        public string TransmissionLossJobRoot { get; set; }

        public string TransmissionLossFileRoot { get; set; }

        public List<string> TimePeriods { get; set; }

        public AppSettings AppSettings { get; set; }

        public NemoFile NemoFile { get; set; }

        public Environment2DData Bathymetry { get; set; }

        List<SoundSourceTimePeriod> _bellhopSources;
        List<SoundSourceTimePeriod> _ramSources;

        public void FindNewJobs(IList<AnalysisPoint> analysisPoints)
        {
            _bellhopSources = new List<SoundSourceTimePeriod>();
            _ramSources = new List<SoundSourceTimePeriod>();
            foreach (var timePeriod in TimePeriods)
            {
                var curJobPath = Path.Combine(TransmissionLossJobRoot, timePeriod);
                var curTLPath = Path.Combine(TransmissionLossFileRoot, timePeriod);
                if (!Directory.Exists(curJobPath)) Directory.CreateDirectory(curJobPath);
                foreach (var analysisPoint in analysisPoints)
                {
                    foreach (var soundSource in analysisPoint.SoundSources)
                    {
                        string runFile;
                        var tlFile = Path.Combine(curTLPath, soundSource.SoundSourceID + ".tlf");
                        switch (soundSource.TransmissionLossAlgorithm)
                        {
                            case TransmissionLossAlgorithm.Bellhop:
                                runFile = Path.Combine(curJobPath, soundSource.SoundSourceID + ".bellhop");
                                if (!IsTLCalculatedOrQueued(tlFile, runFile)) _bellhopSources.Add(new SoundSourceTimePeriod { SoundSource = soundSource, TimePeriod = timePeriod });
                                break;
                            case TransmissionLossAlgorithm.RAM:
                                runFile = Path.Combine(curJobPath, soundSource.SoundSourceID + ".ram");
                                if (!IsTLCalculatedOrQueued(tlFile, runFile)) _ramSources.Add(new SoundSourceTimePeriod { SoundSource = soundSource, TimePeriod = timePeriod });
                                break;
                        }
                    }
                }
            }
        }

        public void CreateJobs(Dispatcher dispatcher)
        {
        }

        static bool IsTLCalculatedOrQueued(string tlFile, string runFile)
        {
            if (File.Exists(tlFile)) return true;
            if (File.Exists(runFile)) return true;
            return false;
        }
    }

    internal class SoundSourceTimePeriod
    {
        internal SoundSource SoundSource { get; set; }
        internal string TimePeriod { get; set; }
    }
}

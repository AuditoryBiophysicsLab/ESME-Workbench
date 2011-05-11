using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using ESME.TransmissionLoss;
using HRC.Utility;

namespace ESME.Validation
{
    public class TransmissionLossValidator
    {
        /// <summary>
        /// Ensure that all Transmission Loss Files in a given directory have matching Transmission Loss Jobs in a second, given directory
        /// </summary>
        /// <param name="transmissionLossJobRoot">The full path to the directory that should be scanned for Transmission Loss Files</param>
        /// <param name="transmissionLossFileRoot">The full path to the directory that should be scanned for Transmission Loss Jobs</param>
        /// <param name="bw"></param>
        public static void ValidateTransmissionLossFiles(string transmissionLossJobRoot, string transmissionLossFileRoot, ImprovedBackgroundWorker bw = null)
        {
            var tlfFiles = Directory.GetFiles(transmissionLossFileRoot);

            var reportProgress = false;
            if ((bw != null) && (bw.WorkerReportsProgress))
            {
                reportProgress = true;
                bw.MinValue = 0;
                bw.MaxValue = tlfFiles.Count() + 1;
                bw.Value = 0;
            }

            foreach (var tlfFile in tlfFiles)
            {
                if (reportProgress) bw.Value++;
                var fileBaseName = Path.GetFileNameWithoutExtension(tlfFile);
                var tljFiles = Directory.GetFiles(transmissionLossJobRoot, fileBaseName + ".*");
                switch (tlfFiles.Length)
                {
                    case 0:
                        // Delete the TLF file because no matching job was found
                        File.Delete(tlfFile);
                        continue;
                    case 1:
                        // There's a file that matches the filename, so further checking is in order
                        break;
                    default:
                        // Ambiguous match.  Delete all matching files
                        File.Delete(tlfFile);
                        foreach (var jobFile in tljFiles)
                            File.Delete(jobFile);
                        continue;
                }
                // Read the header of the TLF file
                var field = TransmissionLossField.Load(tlfFile);
                // Read the header of the runfile
                TransmissionLossAlgorithm algorithm;
                var tljFile = tljFiles.First();
                var runFile = TransmissionLossRunFile.Load(tljFile, out algorithm);
                if (runFile == null)
                {
                    // Algorithm not supported, delete the file and check the next one
                    File.Delete(tljFile);
                    continue;
                }

                // If they do not match, delete the TLF
                if (!runFile.Equals(field)) File.Delete(tlfFile);
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="transmissionLossJobRoot"></param>
        /// <param name="analysisPoints"></param>
        /// <param name="bw"></param>
        public static void ValidateTransmissionLossJobs(string transmissionLossJobRoot, IEnumerable<AnalysisPoint> analysisPoints, ImprovedBackgroundWorker bw = null)
        {
            var tljFiles = Directory.GetFiles(transmissionLossJobRoot);

            var reportProgress = false;
            if ((bw != null) && (bw.WorkerReportsProgress))
            {
                reportProgress = true;
                bw.MinValue = 0;
                bw.MaxValue = tljFiles.Count() + 1;
                bw.Value = 0;
            }

            foreach (var tljFile in tljFiles)
            {
                if (reportProgress) bw.Value++;
                var soundSourceID = Path.GetFileNameWithoutExtension(tljFile);
                TransmissionLossAlgorithm algorithm;
                var runFile = TransmissionLossRunFile.Load(tljFile, out algorithm);
                if (runFile == null)
                {
                    // Algorithm not supported, delete the file and check the next one
                    File.Delete(tljFile);
                    continue;
                }

                var matchingAnalysisPoint = from analysisPoint in analysisPoints
                                            where analysisPoint.AnalysisPointID == runFile.TransmissionLossJob.AnalysisPointID
                                            select analysisPoint;
                switch (matchingAnalysisPoint.Count())
                {
                    case 0:
                        // no matching analysis points found, delete the file and continue
                        File.Delete(tljFile);
                        continue;
                    case 1:
                        // Got one match, more checking is needed
                        break;
                    default:
                        throw new DataException(string.Format("More than one analysis point has ID {0}", runFile.TransmissionLossJob.AnalysisPointID));
                }

                var ap = matchingAnalysisPoint.First();
                var matchingSoundSource = from soundSource in ap.SoundSources
                                          where soundSource.SoundSourceID == soundSourceID
                                          select soundSource;
                switch (matchingSoundSource.Count())
                {
                    case 0:
                        // no matching sound sources found, delete the file and continue
                        File.Delete(tljFile);
                        continue;
                    case 1:
                        // Got one match, more checking is needed
                        break;
                    default:
                        throw new DataException(string.Format("More than one sound source in analysis point {0} has ID {1}", ap.AnalysisPointID, soundSourceID));
                }

                // source is the sound source extracted from the matching analysis point in the experiment
                var source = matchingSoundSource.First();
                if (!source.ShouldBeCalculated)
                {
                    // Sound source should not have been calculated, delete the file and check the next one
                    File.Delete(tljFile);
                    continue;
                }

                if (!source.Equals(runFile.TransmissionLossJob.SoundSource))
                {
                    // The file contents do not match the experiment, delete the file and check the next one
                    File.Delete(tljFile);
                    continue;
                }
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="transmissionLossJobRoot"></param>
        /// <param name="transmissionLossFileRoot"></param>
        /// <param name="bw"></param>
        /// <returns></returns>
        public static Queue<string> GetUncalculatedJobs(string transmissionLossJobRoot, string transmissionLossFileRoot, ImprovedBackgroundWorker bw = null)
        {
            var result = new Queue<string>();
            var tljFiles = Directory.GetFiles(transmissionLossJobRoot);
            var reportProgress = false;
            if ((bw != null) && (bw.WorkerReportsProgress))
            {
                reportProgress = true;
                bw.MinValue = 0;
                bw.MaxValue = tljFiles.Count() + 1;
                bw.Value = 0;
            }
            foreach (var tljFile in tljFiles)
            {
                if (reportProgress) bw.Value++;
                var soundSourceID = Path.GetFileNameWithoutExtension(tljFile);
                TransmissionLossAlgorithm algorithm;
                var runFile = TransmissionLossRunFile.Load(tljFile, out algorithm);
                if (runFile == null)
                {
                    // Algorithm not supported, delete the file and check the next one
                    File.Delete(tljFile);
                    continue;
                }
                var tlfFiles = Directory.GetFiles(transmissionLossFileRoot, soundSourceID + ".*");
                TransmissionLossField field;
                switch (tlfFiles.Length)
                {
                    case 0:
                        // there are no matching TLF files, so the current file needs to be calculated
                        result.Enqueue(tljFile);
                        continue;
                    case 1:
                        // There's a file that matches the filename, so further checking is in order
                        field = TransmissionLossField.Load(tlfFiles.First());
                        if (runFile.Equals(field)) continue;
                        File.Delete(tlfFiles.First());
                        result.Enqueue(tljFile);
                        break;
                    default:
                        // Ambiguous match.  Delete all matching files and recalculate
                        var foundMatch = false;
                        foreach (var tlfFile in tlfFiles)
                        {
                            field = TransmissionLossField.Load(tlfFile);
                            if (!runFile.Equals(field))
                                File.Delete(tlfFile);
                            foundMatch = true;
                            break;
                        }
                        if (!foundMatch) result.Enqueue(tljFile);
                        continue;
                }
            }
            return result;
        }
    }
}

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace ESME.Environment.NAVO
{
    class EnvironmentImporterNew
    {
    }

    public class GDEMImporter
    {
        public static void Import()
        {
            //Logger.Log("Temperature worker starting job");
            TemperatureProgress.JobStarting(job);
            if (Directory.Exists(Path.GetDirectoryName(job.DestinationFilename)))
            {
                //Debug.WriteLine("{0} About to import {1} {2} {3}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType, job.TimePeriod);
                Logger.Log("Temperature worker about to call GDEM.ReadFile");
                SoundSpeedField temperatureField = null;
                try
                {
                    temperatureField = GDEM.ReadFile(GDEM.FindTemperatureFile(job.TimePeriod), "water_temp", job.TimePeriod, job.GeoRect);
                }
                catch (Exception e)
                {
                    Logger.Log("Caught exception from GDEM.ReadFile: {0}\r\nStack trace:\r\n{1}", e.Message, e.StackTrace);
                }
                Logger.Log("Temperature worker back from GDEM.ReadFile");
                var temperature = new SoundSpeed();
                try
                {
                    temperature.SoundSpeedFields.Add(temperatureField);
                }
                catch (Exception e)
                {
                    Logger.Log("Caught exception from SoundSpeedFields.Add: {0}", e.Message);
                }

                Logger.Log("Temperature worker added to sound speed field");
                if (!Directory.Exists(Path.GetDirectoryName(job.DestinationFilename))) Directory.CreateDirectory(Path.GetDirectoryName(job.DestinationFilename));

                //temperature.Save(job.DestinationFilename);
                temperature.Serialize(job.DestinationFilename);
                job.Resolution = 15;
                job.SampleCount = (uint)temperatureField.EnvironmentData.Count;
                //job.CompletionAction(job);
                job.CompletionTask.Start();
                await job.CompletionTask;
            }
            //Debug.WriteLine("{0} Finished importing {1} {2} {3}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType, job.TimePeriod);
            //Logger.Log("Temperature worker job complete", DateTime.Now);
            TemperatureProgress.JobCompleted(job);

        }
    }
}

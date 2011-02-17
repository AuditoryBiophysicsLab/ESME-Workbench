using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Text;
using ESME.Overlay;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.CASS;
using HRC.Navigation;

namespace cassdifftester
{
    class Program
    {
        static void Main(string[] args)
        {
            const string esmeinfile = @"C:\Users\Graham Voysey\Desktop\cass\esme_env_january.dat";
            const string nuwcinfile = @"C:\Users\Graham Voysey\Desktop\cass\env_january.dat";

            var esmeResult = (from packet in CASSFiles.ReadEnvironmentFile(esmeinfile)
                              orderby packet.Location.Latitude_degrees, packet.Location.Longitude_degrees
                              select packet).ToList();



            var nuwcResult = (from packet in CASSFiles.ReadEnvironmentFile(nuwcinfile)
                              orderby packet.Location.Latitude_degrees, packet.Location.Longitude_degrees
                              select packet).ToList();

#if true
            var joinedResult = (from esme in esmeResult
                                from nuwc in nuwcResult
                                where (esme.Location.Equals(nuwc.Location))
                                orderby nuwc.Location.Latitude_degrees, nuwc.Location.Longitude_degrees
                                   select new
                                       {
                                           esme,
                                           nuwc
                                       }).ToList();


            var missingones = (from missing in nuwcResult
                               from present in joinedResult
                               where (!(present.nuwc.Location.Equals(missing.Location)))
                               select new
                                      {
                                          missing,
                                      }).Distinct().ToList();



            if (joinedResult.Count < esmeResult.Count) Console.WriteLine(string.Format("{0} records were extracted from ESME source.  {1} location matches were found in NUWC source. {2} locations were not matched.", esmeResult.Count, joinedResult.Count, nuwcResult.Count - joinedResult.Count)); 
            else Console.WriteLine("all esme records were matched on location");
#endif

            foreach (var esme in esmeResult)
            {
                var matched = false;
                foreach (var nuwc in nuwcResult)
                {
                    //if (esme.Location.Latitude_degrees == 30.75 && esme.Location.Longitude_degrees == -81.25 && nuwc.Location.Latitude_degrees == 30.75 && nuwc.Location.Longitude_degrees == -81.25) //System.Diagnostics.Debugger.Break();
                    if (esme.Location.Equals(nuwc.Location)) matched = true;
                }
                if (!matched) Console.WriteLine(string.Format("esme location {0} is not a nuwc location.", esme.Location));
            }

            Console.ReadLine();
        }
    }


}

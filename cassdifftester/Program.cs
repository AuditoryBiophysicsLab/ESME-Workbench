using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Text;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.CASS;

namespace cassdifftester
{
    class Program
    {
        static void Main(string[] args)
        {
            const string esmeinfile = @"C:\Users\Graham Voysey\Desktop\cass\esme_env_january.dat";
            const string nuwcinfile = @"C:\Users\Graham Voysey\Desktop\cass\env_january.dat";

            var esmeResult = from packet in CASSFiles.ReadEnvironmentFile(esmeinfile)
                              orderby packet.Location.Latitude_degrees , packet.Location.Longitude_degrees
                              select packet;

            var nuwcResult = (from packet in CASSFiles.ReadEnvironmentFile(nuwcinfile)
                              orderby packet.Location.Latitude_degrees , packet.Location.Longitude_degrees
                              select packet).ToList();

            var joinedResult = (from esme in esmeResult
                               from nuwc in nuwcResult
                               where (esme.Location.Equals(nuwc.Location))
                               orderby nuwc.Location.Latitude_degrees , nuwc.Location.Longitude_degrees
                               select esme).ToList();
            if (joinedResult.Count < nuwcResult.Count) Console.WriteLine(string.Format("{0} records were extracted from NUWC source, but only {1} location matches were found. {2} locations were not matched.", nuwcResult.Count, joinedResult.Count, nuwcResult.Count - joinedResult.Count));
            //there are less nuwc results than esme results; our bounding box is bigger.);)
            foreach (var cassPacket in nuwcResult)
            {
                //for each nuwc result, find all esme results that have the same location
                var thisPacket = cassPacket;
                var matchingLocations = esmeResult.Where(packet => thisPacket.Location.Equals(packet.Location)).ToList();
                foreach (var matchingLocation in matchingLocations)
                {
                    for (var i = 0; i < thisPacket.Soundspeeds.Count; i++)
                    {
                        if(!thisPacket.Soundspeeds[i].Equals(matchingLocation.Soundspeeds[i])) throw new DataException("");
                    }
                    
                }
            }
        }
    }


}

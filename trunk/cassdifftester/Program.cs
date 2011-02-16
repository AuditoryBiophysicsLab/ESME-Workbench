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

            var esmeResult = (from packet in CASSFiles.ReadEnvironmentFile(esmeinfile)
                              orderby packet.Location.Latitude_degrees, packet.Location.Longitude_degrees
                              select packet).ToList();

            var nuwcResult = (from packet in CASSFiles.ReadEnvironmentFile(nuwcinfile)
                              orderby packet.Location.Latitude_degrees, packet.Location.Longitude_degrees
                              select packet).ToList();
#if false
            //inner join: return all elements of esmeResult that contain a location present in nuwcResult
            var joinedResult = (from nuwcPacket in nuwcResult
                                join esmePacket in esmeResult on nuwcPacket.Location equals esmePacket.Location into result
                                from packets in result
                                select packets).ToList();


            
#endif

            var joinedResult = from esme in esmeResult
                               from nuwc in nuwcResult
                               where (esme.Location.Equals(nuwc.Location))
                               orderby nuwc.Location.Latitude_degrees , nuwc.Location.Longitude_degrees
                               select esme;
            //there are less nuwc results than esme results; our bounding box is bigger.
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

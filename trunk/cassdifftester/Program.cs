using System;
using System.Collections.Generic;
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

            var eResult = CASSFiles.ReadEnvironmentFile(esmeinfile);
            var nResult = CASSFiles.ReadEnvironmentFile(nuwcinfile);

            //var newEsmeResult = esmeResult.OrderBy(x => x.Location.Latitude_degrees).ToList();
            //var newNuwcResult = nuwcResult.OrderBy(x => x.Location.Latitude_degrees).ToList();

            var esmeResult = from packet in eResult
                             orderby packet.Location.Latitude_degrees , packet.Location.Longitude_degrees 
                             select packet;
            

            var nuwcResult = from packet in nResult
                             orderby packet.Location.Latitude_degrees, packet.Location.Longitude_degrees
                             select packet;


            if (esmeResult.Count().Equals(nuwcResult.Count()))
            {
                for (var i = 0; i < nuwcResult.Count(); i++)
                {
                    //var cassPacket = nuwcResult[i];
                    //if (!cassPacket.IsEqual(esmeResult[i])) Console.WriteLine(@"Not Equal on packet " + i); 
                }
                
            }
            else Console.WriteLine(@"unequal number of packets.");




        }

        static void CompareCASSPacket(CASSPacket thisPacket, CASSPacket thatpacket)
        {
            
        }
    }
}

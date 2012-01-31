using System;
using TEMPer.Communication;

namespace DavesConsoleTester
{
    class Program
    {
        const String CallNumFormat = "000000";
        const String TempFormat = "00.0000";

        static void Main(string[] args)
        {
            int i;

            var comPorts = TEMPerInterface.FindDevices();
            if (comPorts.Length == 0) return;

            var devices = new TEMPerInterface[comPorts.Length];

            for (i = 0; i < comPorts.Length; i++)
                devices[i] = new TEMPerInterface(comPorts[i]);

            for (var callNum = 1; callNum <= 1000; callNum++)
            {
                for (i = 0; i < devices.Length; i++)
                {
                    var tempC = devices[i].ReadTEMP();
                    var tempF = TEMPerInterface.CtoF(tempC);

                    Console.WriteLine("    "
                        + callNum.ToString(CallNumFormat)
                        + "     -     "
                        + devices[i].PortName
                        + "     -     "
                        + tempC.ToString(TempFormat) + " C"
                        + "     -     "
                        + tempF.ToString(TempFormat) + " F");
                }
            }
        }
    }
}

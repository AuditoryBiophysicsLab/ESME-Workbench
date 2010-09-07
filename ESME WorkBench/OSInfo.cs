using System;
using System.Text.RegularExpressions;
using System.Management;
using System.Windows;

namespace ESMEWorkBench
{
    public class OSInfo
    {
        public static string OperatingSystemName { get; private set; }
        public static string OperatingSystemVersion { get; private set; }
        public static int ServicePack { get; private set; }
        public static int Architecture { get; private set; }
        /// <summary>
        /// Gets Operating System Name, Service Pack, and Architecture using WMI with the legacy methods as a fallback
        /// </summary>
        /// <returns>String containing the name of the operating system followed by its service pack (if any) and architecture</returns>
        static OSInfo()
        {
            var objMOS = new ManagementObjectSearcher("SELECT * FROM  Win32_OperatingSystem");

            //Variables to hold our return value
            OperatingSystemName = "";
            ServicePack = 0;
            Architecture = 0;
            OperatingSystemVersion = "";

            try
            {
                foreach (ManagementObject objManagement in objMOS.Get())
                {
                    // Get OS version from WMI - This also gives us the edition
                    var osCaption = objManagement.GetPropertyValue("Caption");
                    if (osCaption == null) continue;
                    // Remove all non-alphanumeric characters so that only letters, numbers, and spaces are left.
                    var osC = Regex.Replace(osCaption.ToString(), "[^A-Za-z0-9 ]", "");
                    //string osC = osCaption.ToString();
                    // If the OS starts with "Microsoft," remove it.  We know that already
                    if (osC.StartsWith("Microsoft"))
                        osC = osC.Substring(9);
                    // If the OS now starts with "Windows," again... useless.  Remove it.
                    if (osC.Trim().StartsWith("Windows"))
                        osC = osC.Trim().Substring(7);
                    // Remove any remaining beginning or ending spaces.
                    OperatingSystemName = osC.Trim();
                    // Only proceed if we actually have an OS version - service pack is useless without the OS version.
                    if (!String.IsNullOrEmpty(OperatingSystemName))
                    {
                        object osServicePack;
                        try
                        {
                            // Get OS service pack from WMI
                            osServicePack = objManagement.GetPropertyValue("ServicePackMajorVersion");
                            if (osServicePack != null && osServicePack.ToString() != "0")
                                ServicePack = int.Parse(osServicePack.ToString());
                            else
                            {
                                // Service Pack not found.  Try built-in Environment class.
                                ServicePack = int.Parse(GetOSServicePackLegacy());
                            }
                        }
                        catch (Exception)
                        {
                            // There was a problem getting the service pack from WMI.  Try built-in Environment class.
                            ServicePack = int.Parse(GetOSServicePackLegacy());
                        }
                    }
                    object osA;
                    try
                    {
                        // Get OS architecture from WMI
                        osA = objManagement.GetPropertyValue("OSArchitecture");
                        if (osA != null)
                        {
                            var osAString = osA.ToString();
                            // If "64" is anywhere in there, it's a 64-bit architectore.
                            Architecture = (osAString.Contains("64") ? 64 : 32);
                        }
                    }
                    catch (Exception) {}
                }
            }
            catch (Exception) {}
            // If WMI couldn't tell us the OS, use our legacy method.
            // We won't get the exact OS edition, but something is better than nothing.
            if (OperatingSystemName == "")
                OperatingSystemName = GetOSLegacy();
            if (OperatingSystemName != null)
            {
                var tmp = OperatingSystemName.Split(new char[]{' '});
                OperatingSystemName = tmp[0];
                if (tmp.Length > 1)
                    OperatingSystemVersion = tmp[1];
            }

            // If WMI couldn't tell us the architecture, use our legacy method.)
            if (Architecture == 0)
                Architecture = GetOSArchitectureLegacy();
            
            //Console.WriteLine("Operating system: \"{0}\"", os);
            //Console.WriteLine("    Service pack: \"{0}\"", servicePack);
            //Console.WriteLine("    Architecture: {0}", osArch);
        }

        /// <summary>
        /// Gets Operating System Name using .Net's Environment class.
        /// </summary>
        /// <returns>String containing the name of the operating system followed by its service pack (if any)</returns>
        private static string GetOSLegacy()
        {
            //Get Operating system information.
            var os = Environment.OSVersion;
            //Get version information about the os.
            var vs = os.Version;

            //Variable to hold our return value
            string operatingSystem = "";

            switch (os.Platform)
            {
                case PlatformID.Win32Windows:
                    switch (vs.Minor)
                    {
                        case 0:
                            operatingSystem = "95";
                            break;
                        case 10:
                            operatingSystem = vs.Revision.ToString() == "2222A" ? "98SE" : "98";
                            break;
                        case 90:
                            operatingSystem = "Me";
                            break;
                        default:
                            break;
                    }
                    break;
                case PlatformID.Win32NT:
                    switch (vs.Major)
                    {
                        case 3:
                            operatingSystem = "NT 3.51";
                            break;
                        case 4:
                            operatingSystem = "NT 4.0";
                            break;
                        case 5:
                            operatingSystem = vs.Minor == 0 ? "2000" : "XP";
                            break;
                        case 6:
                            operatingSystem = vs.Minor == 0 ? "Vista" : "7";
                            break;
                        default:
                            break;
                    }
                    break;
            }
            //Make sure we actually got something in our OS check
            //We don't want to just return " Service Pack 2"
            //That information is useless without the OS version.
            if (operatingSystem != "")
            {
                //Got something.  Let's see if there's a service pack installed.
                operatingSystem += GetOSServicePackLegacy();
            }
            //Return the information we've gathered.
            return operatingSystem;
        }

        /// <summary>
        /// Gets the installed Operating System Service Pack using .Net's Environment class.
        /// </summary>
        /// <returns>String containing the operating system's installed service pack (if any)</returns>
        private static string GetOSServicePackLegacy()
        {
            // Get service pack from Environment Class
            var sp = Environment.OSVersion.ServicePack;
            if (!string.IsNullOrEmpty(sp) && sp != " ")
                return sp;

            // No service pack.  Return an empty string
            return "";
        }

        /// <summary>
        /// Gets Operating System Architecture.  This does not tell you if the program in running in
        /// 32- or 64-bit mode or if the CPU is 64-bit capable.  It tells you whether the actual Operating
        /// System is 32- or 64-bit.
        /// </summary>
        /// <returns>Int containing 32 or 64 representing the number of bits in the OS Architecture</returns>
        private static int GetOSArchitectureLegacy()
        {
            var pa = Environment.GetEnvironmentVariable("PROCESSOR_ARCHITECTURE");
            return ((String.IsNullOrEmpty(pa) || String.Compare(pa, 0, "x86", 0, 3, true) == 0) ? 32 : 64);
        }
    }
}
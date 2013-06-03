using System.Management;

namespace HRC.Utility
{
    public class CpuInfo : PropertyChangedBase
    {
        public CpuInfo()
        {
            _managementObjectSearcher = new ManagementObjectSearcher("Select * from Win32_Processor");
            var managementObjectCollection = _managementObjectSearcher.Get();
            PhysicalCpuCount = (uint)managementObjectCollection.Count;
            CpuDescriptions = new string[PhysicalCpuCount];
            var itemIndex = 0;
            foreach (var item in managementObjectCollection)
            {
                PhysicalCores += ((uint)item["NumberOfCores"]);
                LogicalCores += ((uint)item["NumberOfLogicalProcessors"]);
                CpuDescriptions[itemIndex] = ((string)item["Description"]);
                itemIndex++;
            }
        }

        readonly ManagementObjectSearcher _managementObjectSearcher;
        public uint PhysicalCpuCount { get; private set; }
        public uint PhysicalCores { get; private set; }
        public uint LogicalCores { get; private set; }
        public string[] CpuDescriptions { get; private set; }

        public float CurrentLoad
        {
            get
            {
                var managementObjectCollection = _managementObjectSearcher.Get();
                var totalLoad = 0f;
                foreach (var item in managementObjectCollection)
                    totalLoad += ((ushort)item["LoadPercentage"]);
                return totalLoad / PhysicalCpuCount;
            }
        }
    }
}

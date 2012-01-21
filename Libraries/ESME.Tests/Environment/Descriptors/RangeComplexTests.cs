using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Security.Permissions;
using System.Text;
using System.Windows.Threading;
using ESME.Data;
using ESME.Environment.Descriptors;
using HRC.Navigation;
using NUnit.Framework;

namespace ESME.Tests.Environment.Descriptors
{
    public class RangeComplexTests
    {
        [Test, TestCaseSource("_createCases"), Timeout(50000), RequiresSTA]
        public void Create(string simAreaPath, RangeComplexMetadata rangeComplexMetadata, IEnumerable<Geo> opAreaLimits, IEnumerable<Geo> simAreaLimits)
        {
            var curSimAreaPath = Path.Combine(simAreaPath, rangeComplexMetadata.Name);
            if (Directory.Exists(curSimAreaPath)) Directory.Delete(curSimAreaPath, true);
            Globals.AppSettings = new AppSettings
            {
                NAVOConfiguration =
                {
                    BSTDirectory  = @"S:\OAML Data Sources\BST\Sediments2.0_QAV_Analysis\Sediments\Version2.0\databases\hfevav2.h5",
                    DBDBDirectory = @"S:\OAML Data Sources\DBDBV_v5.4\data\dbdbv5_level0c_0.h5",
                    DBDBEXEPath   = @"S:\OAML Data Sources\DBDBV_v5.4\bin\Windows\dbv5_command.exe",
                    GDEMDirectory = @"S:\OAML Data Sources\GDEM-V\GDEM-V_3.0_20031016\Modified for Environment Builder",
                    SMGCDirectory = @"S:\OAML Data Sources\SMGC",
                }
            };
            RangeComplex.Create(simAreaPath, rangeComplexMetadata, opAreaLimits, simAreaLimits, Dispatcher.CurrentDispatcher);
            var frame = new DispatcherFrame();
            Dispatcher.PushFrame(frame);
        }

        static object[] _createCases = 
        {
            new object[]
            {
                @"C:\Projects\ESME Deliverables\Libraries\ESME.Tests\Test Files\Simulation Areas\Jacksonville\Sim Areas", 
                new RangeComplexMetadata("JaxTest", 0, 30.300000,-80.100000, 0, "JaxTestOpArea.ovr", "JaxTestSimArea.ovr"),
                new List<Geo> { new Geo(29.3590, -79.2195), new Geo(31.1627, -79.2195), new Geo(31.1627, -81.2789), new Geo(30.1627, -81.2789), new Geo(29.3590, -80.8789), new Geo(29.3590, -79.2195)},
                new List<Geo> { new Geo(29.3590, -79.2195), new Geo(31.1627, -79.2195), new Geo(31.1627, -81.2789), new Geo(30.1627, -81.2789), new Geo(29.3590, -80.8789), new Geo(29.3590, -79.2195)},
            },
        };
    }

    public static class DispatcherUtil
    {
        [SecurityPermissionAttribute(SecurityAction.Demand, Flags = SecurityPermissionFlag.UnmanagedCode)]
        public static void DoEvents()
        {
            var frame = new DispatcherFrame();
            Dispatcher.CurrentDispatcher.BeginInvoke(DispatcherPriority.Background,
                new DispatcherOperationCallback(ExitFrame), frame);
            Dispatcher.PushFrame(frame);
        }

        private static object ExitFrame(object frame)
        {
            ((DispatcherFrame)frame).Continue = false;
            return null;
        }
    }
}

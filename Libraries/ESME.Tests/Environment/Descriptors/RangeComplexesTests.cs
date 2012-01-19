using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using ESME.Data;
using ESME.Environment.Descriptors;
using NUnit.Framework;

namespace ESME.Tests.Environment.Descriptors
{
    [TestFixture(@"C:\Projects\ESME Deliverables\Libraries\ESME.Tests\Test Files\Simulation Areas",
                 @"Jacksonville",
                 @"S:\OAML Data Sources\BST\Sediments2.0_QAV_Analysis\Sediments\Version2.0\databases\hfevav2.h5",
                 @"S:\OAML Data Sources\DBDBV_v5.4\data\dbdbv5_level0c_0.h5",
                 @"S:\OAML Data Sources\DBDBV_v5.4\bin\Windows\dbv5_command.exe",
                 @"S:\OAML Data Sources\GDEM-V\GDEM-V_3.0_20031016\Modified for Environment Builder",
                 @"S:\OAML Data Sources\SMGC")]
    public class RangeComplexesTests
    {
        readonly string _simAreaBase, _simAreaName, _bstDatabasePath, _dbdbPath, _dbdbExePath, _gdemPath, _smgcPath;
        public RangeComplexesTests(string simAreaBase, string simAreaName, string bstDatabasePath, string dbdbPath, string dbdbExePath, string gdemPath, string smgcPath)
        {
            if (!Directory.Exists(simAreaBase)) throw new DirectoryNotFoundException(simAreaBase);
            if (!Directory.Exists(Path.Combine(simAreaBase, simAreaName))) throw new DirectoryNotFoundException(Path.Combine(simAreaBase, simAreaName));
            if (!Directory.Exists(Path.Combine(simAreaBase, simAreaName, "Sim Areas"))) throw new DirectoryNotFoundException(Path.Combine(simAreaBase, simAreaName, "Sim Areas"));
            if (!Directory.Exists(Path.Combine(simAreaBase, simAreaName, "Sim Areas", simAreaName))) throw new DirectoryNotFoundException(Path.Combine(simAreaBase, simAreaName, "Sim Areas", simAreaName));
            if (!File.Exists(Path.Combine(simAreaBase, simAreaName, "Sim Areas", "SimAreas.csv"))) throw new FileNotFoundException(Path.Combine(simAreaBase, simAreaName, "Sim Areas", "SimAreas.csv"));
            if (!File.Exists(bstDatabasePath)) throw new FileNotFoundException(bstDatabasePath);
            if (!File.Exists(dbdbPath)) throw new FileNotFoundException(dbdbPath);
            if (!File.Exists(dbdbExePath)) throw new FileNotFoundException(dbdbExePath);
            if (!Directory.Exists(gdemPath)) throw new DirectoryNotFoundException(gdemPath);
            if (!Directory.Exists(smgcPath)) throw new DirectoryNotFoundException(smgcPath);
            _simAreaBase = simAreaBase;
            _simAreaName = simAreaName;
            _bstDatabasePath = bstDatabasePath;
            _dbdbPath = dbdbPath;
            _dbdbExePath = dbdbExePath;
            _gdemPath = gdemPath;
            _smgcPath = smgcPath;
        }

        [TestFixtureSetUp]
        public void Initialize()
        {
            var simAreaPath = Path.Combine(_simAreaBase, _simAreaName)
            var areasPath = Path.Combine(_simAreaBase, "Areas");
            var bathymetryPath = Path.Combine(_simAreaBase, "Bathymetry");
            var dataPath = Path.Combine(_simAreaBase, "Data");
            var environmentPath = Path.Combine(_simAreaBase, "Environment");
            var geographicAreasPath = Path.Combine(_simAreaBase, "GeographicAreas");
            var imagesPath = Path.Combine(_simAreaBase, "Images");
            var speciesPath = Path.Combine(_simAreaBase, "Species");

            Globals.AppSettings = new AppSettings
            {
                NAVOConfiguration =
                    {
                        BSTDirectory = _bstDatabasePath,
                        DBDBDirectory = _dbdbPath,
                        DBDBEXEPath = _dbdbExePath,
                        GDEMDirectory = _gdemPath,
                        SMGCDirectory = _smgcPath,
                    }
            };

            if (!Directory.Exists(areasPath)) throw new DirectoryNotFoundException(areasPath);
            Directory.Delete(bathymetryPath, true);
            Directory.Delete(dataPath, true);
            Directory.Delete(environmentPath, true);
            Directory.Delete(geographicAreasPath, true);
            Directory.Delete(imagesPath, true);
            Directory.Delete(speciesPath, true);

            var simAreaFilePath = Path.Combine(_simAreaBase, "SimAreas.csv");
            var rangeComplexes = RangeComplexes.Singleton;
            rangeComplexes.ReadRangeComplexFile(simAreaFilePath);
            if (!Directory.Exists(areasPath)) throw new DirectoryNotFoundException(areasPath);
            if (!Directory.Exists(bathymetryPath)) throw new DirectoryNotFoundException(bathymetryPath);
            if (!Directory.Exists(dataPath)) throw new DirectoryNotFoundException(dataPath);
            if (!Directory.Exists(environmentPath)) throw new DirectoryNotFoundException(environmentPath);
            if (!Directory.Exists(geographicAreasPath)) throw new DirectoryNotFoundException(geographicAreasPath);
            if (!Directory.Exists(imagesPath)) throw new DirectoryNotFoundException(imagesPath);
            if (!Directory.Exists(speciesPath)) throw new DirectoryNotFoundException(speciesPath);
        }
    }
}

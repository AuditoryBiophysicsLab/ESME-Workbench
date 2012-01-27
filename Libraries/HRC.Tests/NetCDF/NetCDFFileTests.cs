using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using NUnit.Framework;
using HRC.NetCDF;

namespace HRC.Tests.NetCDF
{
    public class NetCDFFileTests
    {
        // Note: The test file should be 'sgdemv3s06.nc', which is the January Salinity data file from the GDEM v3 disc from NAVOCEANO
        // Many of the tests depend on the particular data in that file and will fail if the file given here is something else.
        const string TestFileThatExists = @"S:\OAML Data Sources\GDEM-V\GDEM-V_3.0_20031016\Modified for Environment Builder\sgdemv3s06.nc";

        [Test]
        public void OpenNonexistentFile()
        {
            Assert.Throws<FileNotFoundException>(() => NetCDFFile.Open("nonexistent.nc"));
        }

        [Test]
        public void OpenExistingFile()
        {
            var ncFile = NetCDFFile.Open(TestFileThatExists);
            Assert.NotNull(ncFile);
            ncFile.Close();
        }

        [Test]
        public void DumpExistingFile()
        {
            var ncFile = NetCDFFile.Open(TestFileThatExists);
            Assert.NotNull(ncFile);
            Console.WriteLine(ncFile.Dump());
        }

        [Test]
        public void ReadJuneGDEMSalinityFile()
        {
            StringAssert.EndsWith("sgdemv3s06.nc", TestFileThatExists, "The passed-in file does not appear to be the June GDEMv3 Salinity file");
            var ncFile = NetCDFFile.Open(TestFileThatExists);
            Assert.NotNull(ncFile);

            Assert.AreEqual(9, ncFile.Attributes.Count);

            var ncAttChar = (NcAttChar)ncFile.Attributes[0];
            StringAssert.AreEqualIgnoringCase("CONVENTION", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("NAVO_netcdf_v1.0", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncFile.Attributes[1];
            StringAssert.AreEqualIgnoringCase("INSTITUTION", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("Naval Oceanographic Office", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncFile.Attributes[2];
            StringAssert.AreEqualIgnoringCase("CONTACT", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("NAVO, Code N312", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncFile.Attributes[3];
            StringAssert.AreEqualIgnoringCase("HISTORY", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("created: 01-Oct-2002 14:46:18", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncFile.Attributes[4];
            StringAssert.AreEqualIgnoringCase("DESCRIPTION", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("GDEMV 3.0", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncFile.Attributes[5];
            StringAssert.AreEqualIgnoringCase("CLASSIFICATION_LEVEL", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("UNCLASSIFIED", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncFile.Attributes[6];
            StringAssert.AreEqualIgnoringCase("DISTRIBUTION_STATEMENT", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("DISTRIBUTION STATEMENT A:  APPROVED FOR PUBLIC RELEASE: DISTRIBUTION IS UNLIMITED", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncFile.Attributes[7];
            StringAssert.AreEqualIgnoringCase("DOWNGRADE_DATE", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("N/A", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncFile.Attributes[8];
            StringAssert.AreEqualIgnoringCase("CLASSIFICATION_AUTHORITY", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("N/A", ncAttChar.Value);

            var ncVarDouble = (NcVarDouble)ncFile.Variables[0];
            StringAssert.AreEqualIgnoringCase("lat", ncVarDouble.Name);
            Assert.AreEqual(1, ncVarDouble.Dimensions.Count);
            Assert.AreEqual(689, ncVarDouble.Dimensions[0].Length);

            ncAttChar = (NcAttChar)ncVarDouble.Attributes[0];
            StringAssert.AreEqualIgnoringCase("long_name", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("Latitude", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncVarDouble.Attributes[1];
            StringAssert.AreEqualIgnoringCase("units", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("degrees_north", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncVarDouble.Attributes[2];
            StringAssert.AreEqualIgnoringCase("FORTRAN_format", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("e13.6", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncVarDouble.Attributes[3];
            StringAssert.AreEqualIgnoringCase("point_spacing", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("even", ncAttChar.Value);

            var ncAttInt = (NcAttInt)ncVarDouble.Attributes[4];
            StringAssert.AreEqualIgnoringCase("Navo_code", ncAttInt.Name);
            Assert.AreEqual(1, ncAttInt.Value.Length);
            Assert.AreEqual(1, ncAttInt.Value[0]);

            var latitudes = new List<double>();
            for (var index = 0; index < 689; index++) latitudes.Add(ncVarDouble[(uint)index]);
            var lat26N = latitudes.IndexOf(26);
            Debug.WriteLine("Latitude 26N is at index {0}", lat26N);

            ncVarDouble = (NcVarDouble)ncFile.Variables[1];
            StringAssert.AreEqualIgnoringCase("lon", ncVarDouble.Name);
            Assert.AreEqual(1, ncVarDouble.Dimensions.Count);
            Assert.AreEqual(1440, ncVarDouble.Dimensions[0].Length);

            ncAttChar = (NcAttChar)ncVarDouble.Attributes[0];
            StringAssert.AreEqualIgnoringCase("long_name", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("Longitude", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncVarDouble.Attributes[1];
            StringAssert.AreEqualIgnoringCase("units", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("degrees_east", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncVarDouble.Attributes[2];
            StringAssert.AreEqualIgnoringCase("FORTRAN_format", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("e13.6", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncVarDouble.Attributes[3];
            StringAssert.AreEqualIgnoringCase("point_spacing", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("even", ncAttChar.Value);

            ncAttInt = (NcAttInt)ncVarDouble.Attributes[4];
            StringAssert.AreEqualIgnoringCase("Navo_code", ncAttInt.Name);
            Assert.AreEqual(1, ncAttInt.Value.Length);
            Assert.AreEqual(2, ncAttInt.Value[0]);

            var longitudes = new List<double>();
            for (var index = 0; index < 1440; index++) longitudes.Add(ncVarDouble[(uint)index]);
            var lon53E = longitudes.IndexOf(53);
            Debug.WriteLine("Longitude 53E is at index {0}", lon53E);

            ncVarDouble = (NcVarDouble)ncFile.Variables[2];
            StringAssert.AreEqualIgnoringCase("depth", ncVarDouble.Name);
            Assert.AreEqual(1, ncVarDouble.Dimensions.Count);
            Assert.AreEqual(78, ncVarDouble.Dimensions[0].Length);

            ncAttChar = (NcAttChar)ncVarDouble.Attributes[0];
            StringAssert.AreEqualIgnoringCase("long_name", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("Depth", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncVarDouble.Attributes[1];
            StringAssert.AreEqualIgnoringCase("units", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("meters", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncVarDouble.Attributes[2];
            StringAssert.AreEqualIgnoringCase("FORTRAN_format", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("e13.6", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncVarDouble.Attributes[3];
            StringAssert.AreEqualIgnoringCase("point_spacing", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("uneven", ncAttChar.Value);

            ncAttInt = (NcAttInt)ncVarDouble.Attributes[4];
            StringAssert.AreEqualIgnoringCase("Navo_code", ncAttInt.Name);
            Assert.AreEqual(1, ncAttInt.Value.Length);
            Assert.AreEqual(5, ncAttInt.Value[0]);

            ncVarDouble = (NcVarDouble)ncFile.Variables[3];
            StringAssert.AreEqualIgnoringCase("time", ncVarDouble.Name);
            Assert.AreEqual(1, ncVarDouble.Dimensions.Count);
            Assert.AreEqual(1, ncVarDouble.Dimensions[0].Length);

            ncAttChar = (NcAttChar)ncVarDouble.Attributes[0];
            StringAssert.AreEqualIgnoringCase("FORTRAN_format", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("F10.2", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncVarDouble.Attributes[1];
            StringAssert.AreEqualIgnoringCase("units", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("hour since 0000-01-01 00:00:00", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncVarDouble.Attributes[2];
            StringAssert.AreEqualIgnoringCase("type", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("EVEN", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncVarDouble.Attributes[3];
            StringAssert.AreEqualIgnoringCase("time_origin", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("1-Jan-0000 00:00:00", ncAttChar.Value);

            ncAttInt = (NcAttInt)ncVarDouble.Attributes[4];
            StringAssert.AreEqualIgnoringCase("Navo_code", ncAttInt.Name);
            Assert.AreEqual(1, ncAttInt.Value.Length);
            Assert.AreEqual(13, ncAttInt.Value[0]);

            var ncVarShort = (NcVarShort)ncFile.Variables[4];
            StringAssert.AreEqualIgnoringCase("salinity", ncVarShort.Name);
            Assert.AreEqual(3, ncVarShort.Dimensions.Count);
            Assert.AreEqual(78, ncVarShort.Dimensions[0].Length);
            Assert.AreEqual(689, ncVarShort.Dimensions[1].Length);
            Assert.AreEqual(1440, ncVarShort.Dimensions[2].Length);

            var ncAttShort = (NcAttShort)ncVarShort.Attributes[0];
            StringAssert.AreEqualIgnoringCase("_FillValue", ncAttShort.Name);
            Assert.AreEqual(1, ncAttShort.Value.Length);
            Assert.AreEqual(-32000, ncAttShort.Value[0]);

            ncAttShort = (NcAttShort)ncVarShort.Attributes[1];
            StringAssert.AreEqualIgnoringCase("missing_value", ncAttShort.Name);
            Assert.AreEqual(1, ncAttShort.Value.Length);
            Assert.AreEqual(-32000, ncAttShort.Value[0]);

            ncAttChar = (NcAttChar)ncVarShort.Attributes[2];
            StringAssert.AreEqualIgnoringCase("long_name", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("Salinity", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncVarShort.Attributes[3];
            StringAssert.AreEqualIgnoringCase("units", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("psu", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncVarShort.Attributes[4];
            StringAssert.AreEqualIgnoringCase("FORTRAN_format", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("e13.6", ncAttChar.Value);

            ncAttChar = (NcAttChar)ncVarShort.Attributes[5];
            StringAssert.AreEqualIgnoringCase("coordinate_system", ncAttChar.Name);
            StringAssert.AreEqualIgnoringCase("orthogonal_constant-spacing", ncAttChar.Value);

            var ncAttFloat = (NcAttFloat)ncVarShort.Attributes[6];
            StringAssert.AreEqualIgnoringCase("scale_factor", ncAttFloat.Name);
            Assert.AreEqual(1, ncAttFloat.Value.Length);
            Assert.AreEqual(0.001f, ncAttFloat.Value[0]);
            var scaleFactor = ncAttFloat.Value[0];

            ncAttFloat = (NcAttFloat)ncVarShort.Attributes[7];
            StringAssert.AreEqualIgnoringCase("add_offset", ncAttFloat.Name);
            Assert.AreEqual(1, ncAttFloat.Value.Length);
            Assert.AreEqual(15, ncAttFloat.Value[0]);
            var addOffset = ncAttFloat.Value[0];

            ncAttInt = (NcAttInt)ncVarShort.Attributes[8];
            StringAssert.AreEqualIgnoringCase("Navo_code", ncAttInt.Name);
            Assert.AreEqual(1, ncAttInt.Value.Length);
            Assert.AreEqual(16, ncAttInt.Value[0]);

            // The following test locations, depths and expected results are extracted from the official GDEM documentation file
            Assert.That(38.660, Is.EqualTo(addOffset + scaleFactor * ncVarShort[ 0, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(38.678, Is.EqualTo(addOffset + scaleFactor * ncVarShort[ 1, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(38.697, Is.EqualTo(addOffset + scaleFactor * ncVarShort[ 2, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(38.737, Is.EqualTo(addOffset + scaleFactor * ncVarShort[ 3, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(38.794, Is.EqualTo(addOffset + scaleFactor * ncVarShort[ 4, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(38.867, Is.EqualTo(addOffset + scaleFactor * ncVarShort[ 5, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(39.177, Is.EqualTo(addOffset + scaleFactor * ncVarShort[ 6, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(39.482, Is.EqualTo(addOffset + scaleFactor * ncVarShort[ 7, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(39.652, Is.EqualTo(addOffset + scaleFactor * ncVarShort[ 8, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(39.776, Is.EqualTo(addOffset + scaleFactor * ncVarShort[ 9, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(39.830, Is.EqualTo(addOffset + scaleFactor * ncVarShort[10, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(39.879, Is.EqualTo(addOffset + scaleFactor * ncVarShort[11, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(39.921, Is.EqualTo(addOffset + scaleFactor * ncVarShort[12, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(39.983, Is.EqualTo(addOffset + scaleFactor * ncVarShort[13, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(39.985, Is.EqualTo(addOffset + scaleFactor * ncVarShort[14, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(40.001, Is.EqualTo(addOffset + scaleFactor * ncVarShort[15, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(40.004, Is.EqualTo(addOffset + scaleFactor * ncVarShort[16, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(40.000, Is.EqualTo(addOffset + scaleFactor * ncVarShort[17, (uint)lat26N, (uint)lon53E]).Within(0.00001));
            Assert.That(40.011, Is.EqualTo(addOffset + scaleFactor * ncVarShort[18, (uint)lat26N, (uint)lon53E]).Within(0.00001));
        }
    }
}

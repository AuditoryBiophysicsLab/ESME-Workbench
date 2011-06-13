﻿using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using ESME.Environment;
using ESME.Environment.NAVO;
using HRC.Navigation;
using NetCDF;

namespace ImportNetCDF
{
    internal class Program
    {
        static void Main(string[] args)
        {
            var lonVarName = "";
            var latVarName = "";
            var depthVarName = "";
            var dataVarName = "";
            var missingValueAttName = "";
            var scaleFactorAttName = "";
            var offsetValueAttName = "";
            var outputDataFileName = "";
            var netCdfFileName = "";
            var north = float.NaN;
            var south = float.NaN;
            var east = float.NaN;
            var west = float.NaN;
            var month = (NAVOTimePeriod) 0;

            if (args.Length < 1)
            {
                Usage();
                return;
            }
            for (var i = 0; i < args.Length; i++)
            {
                switch (args[i])
                {
                    case "-in":
                    case "-input":
                        netCdfFileName = args[++i];
                        break;
                    case "-lon": //the longitude variable name in the NetCDF file
                    case "-longitude":
                        lonVarName = args[++i];
                        break;
                    case "-lat": //the latitude variable name
                    case "-latitude":
                        latVarName = args[++i];
                        break;
                    case "-dep":
                    case "-depth":
                        depthVarName = args[++i]; //the depth variable name
                        break;
                    case "-data":
                        dataVarName = args[++i]; //the data variable name (like "water_temp" or "salinity")
                        break;
                    case "-mv":
                    case "-miss":
                    case "-missing":
                    case "-missingvalue":
                        missingValueAttName = args[++i]; //like "missing_value"
                        break;
                    case "-sf":
                    case "-scale":
                    case "-scalefactor":
                        scaleFactorAttName = args[++i]; //like "scale_factor"
                        break;
                    case "-offset":
                        offsetValueAttName = args[++i]; //like "add_offset"
                        break;
                    case "-north":
                        north = float.Parse(args[++i]);
                        break;
                    case "-south":
                        south = float.Parse(args[++i]);
                        break;
                    case "-east":
                        east = float.Parse(args[++i]);
                        break;
                    case "-west":
                        west = float.Parse(args[++i]);
                        break;
                    case "-output":
                    case "-out":
                        outputDataFileName = args[++i];
                        break;
                    case "-month":
                    case "-mon":
                        month = (NAVOTimePeriod)Enum.Parse(typeof(NAVOTimePeriod), args[++i], true);
                        break;
                    default:
                        Usage();
                        return;
                }
            }

            if ((netCdfFileName == "") || (outputDataFileName == "") || (lonVarName == "") || (latVarName == "") || (dataVarName == "") || (depthVarName == "") || (missingValueAttName == "") || (scaleFactorAttName == "") || (offsetValueAttName == "") || (month == 0))
            {
                Usage();
                return;
            }

            if (!File.Exists(netCdfFileName)) throw new FileNotFoundException("ImportNetCDF: File {0} not found", netCdfFileName);

            ImportNetCdf(netCdfFileName, dataVarName, lonVarName, latVarName, depthVarName, missingValueAttName, scaleFactorAttName, offsetValueAttName, outputDataFileName, north, south, east, west, month);

            Console.WriteLine(@"success!");
        }

        static void ImportNetCdf(string netCdfFileName, string dataVarName, string lonVarName, string latVarName, string depthVarName, string missingValueAttName, string scaleFactorAttName, string offsetValueAttName, string outputDataFileName, float north, float south, float east, float west, NAVOTimePeriod month)
        {
            var myFile = new NcFile(netCdfFileName);
            int lonIndex, depthIndex;
            short missingValue;

            myFile.LoadAllData();

            var lonVar = myFile.Variables[lonVarName];
            var latVar = myFile.Variables[latVarName];
            var depthVar = myFile.Variables[depthVarName];
            var dataVar = myFile.Variables[dataVarName];
            
            var lons = new float[lonVar.ElementCount];
            for (var i = 0; i < lons.Length; i++) lons[i] = lonVar.GetFloat(i);
            var lats = new float[latVar.ElementCount];
            for (var i = 0; i < lats.Length; i++) lats[i] = latVar.GetFloat(i);
            
            if (lons.First() > west) west += 360;
            if (lons.Last() < west) west -= 360;
            if (lons.First() > east) east += 360;
            if (lons.Last() < east) east -= 360;

            var lonMap = new List<AxisMap>();
            var latMap = new List<AxisMap>();
            for (var i = 0; i < lons.Length; i++)
            {
                var temp = lonVar.GetFloat(i);
                if ((temp >= west) && (temp <= east)) lonMap.Add(new AxisMap(temp, i));
            }
            for (var i = 0; i < lats.Length; i++)
            {
                var temp = latVar.GetFloat(i);
                if (temp >= south && temp <= north) latMap.Add(new AxisMap(temp, i));
            }
            var selectedLons = lonMap.Select(x => x.Value).ToArray();
            var selectedLats = latMap.Select(x => x.Value).ToArray();

            Console.WriteLine(@"Initializing output file {0}...", Path.GetFileName(outputDataFileName));

            var depthCount = (int) depthVar.Dimensions[0].Size;
            var depths = new float[depthCount];
            for (depthIndex = 0; depthIndex < depthCount; depthIndex++) depths[depthIndex] = depthVar.GetFloat(depthIndex);

            var latCount = selectedLats.Length;
            var lonCount = selectedLons.Length;

            var scaleFactor = 1.0f;
            var addOffset = 0.0f;
            if (!short.TryParse(missingValueAttName, out missingValue))
                if (missingValueAttName != String.Empty) missingValue = dataVar.Attributes[missingValueAttName].GetShort(0);

            if (scaleFactorAttName != String.Empty) scaleFactor = dataVar.Attributes[scaleFactorAttName].GetFloat(0);
            if (offsetValueAttName != String.Empty) addOffset = dataVar.Attributes[offsetValueAttName].GetFloat(0);

            Console.WriteLine(@"Importing source file {0}...", Path.GetFileName(netCdfFileName));

            var writer = new SoundSpeed();
            var newField = new SoundSpeedField { TimePeriod = month };
            writer.SoundSpeedFields.Add(newField);

            for (lonIndex = 0; lonIndex < lonCount; lonIndex++)
            {
                var lon = lonMap[lonIndex].Value;
                var wrappedLon = lon;
                while (wrappedLon > 180) wrappedLon -= 360;
                while (wrappedLon < -180) wrappedLon += 360;

                var lonSourceIndex = lonMap[lonIndex].Index;
                for (var latIndex = 0; latIndex < latCount; latIndex++)
                {
                    var lat = latMap[latIndex].Value;
                    var latSourceIndex = latMap[latIndex].Index;
                    var newProfile = new SoundSpeedProfile(new EarthCoordinate(lat, wrappedLon));
                    for (depthIndex = 0; depthIndex < depthCount; depthIndex++)
                    {
                        var curValue = dataVar.GetShort(depthIndex, latSourceIndex, lonSourceIndex);
                        if (curValue == missingValue) break;
                        newProfile.Data.Add(new DepthValuePair<float>(depths[depthIndex], ((curValue) * scaleFactor) + addOffset));
                    }
                    if (newProfile.Data.Count > 0) newField.EnvironmentData.Add(newProfile);
                }
            }
            Console.WriteLine(@"Saving imported data ... ");
            writer.Save(outputDataFileName);
            Console.WriteLine(@"done");
        }

        static void Usage()
        {
            Console.WriteLine(
                "Usage: ImportNetCDF -input <netCDFFileName>\n" +
                "                    -data <DataVariable>\n" +
                "                    -lon <LongitudeVariable>\n" +
                "                    -lat <LatitudeVariable>\n" +
                "                    -depth <DepthVariable> \n" +
                "                    -missingvalue <MissingValueAttribute> \n" +
                "                    -scalefactor <ScaleFactorAttribute> \n" +
                "                    -offset <OffsetValueAttribute> \n" +
                "                    -output <OutputFileName> \n" +
                "                    -month <MonthName>\n" +
                "\n" +
                "Where: <netCDFFileName> is the full path to an UNCOMPRESSED NetCDF file,\n" +
                "       typically provided by OAML for the GDEM-V and the DBDB-V datasets.\n" +
                "\n" +
                "       <DataVariable> is the name of the NetCDF variable that contains the\n" +
                "       'payload' data.\n" +
                "\n" +
                "       <LongitudeVariable> is the name of the NetCDF variable that contains the\n" +
                "       array of longitudes present in the payload data\n" +
                "\n" +
                "       <LatitudeVariable> is the name of the NetCDF variable that contains the\n" +
                "       array of latitudes present in the payload data\n" +
                "\n" +
                "       <DepthVariable> is the name of the NetCDF variable that contains the\n" +
                "       array of depths present in the payload data.\n" +
                "\n" +
                "       <MissingValueAttribute> is the name of the NetCDF attribute of\n" +
                "       <DataVariable> that contains the value used to represent a 'no data'\n" +
                "       condition in the payload dataset.\n" +
                "\n" +
                "       <ScaleFactorAttribute> is the name of the NetCDF attribute of\n" +
                "       <DataVariable> that contains the value used to scale the data into\n" +
                "       its final range.\n" +
                "\n" +
                "       <OffsetValueAttribute> is the name of the NetCDF attribute of\n" +
                "       <DataVariable> that contains the value used to offset the scaled data\n" +
                "       to its final value.\n" +
                "\n" +
                "       <OutputFileName> is the name of the XML file that will contain the data\n" +
                "       imported from the specified NetCDF file.\n" +
                "\n" +
                "       <MonthName> is the time period represented <netCDFFileName>.\n" +
                "\n");
        }
    }
}
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml.Serialization;
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
            var netCDFFileName = "";
            var north = float.NaN;
            var south = float.NaN;
            var east = float.NaN;
            var west = float.NaN;

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
                        netCDFFileName = args[++i];
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
                    case "-dataout":
                        outputDataFileName = args[++i];
                        break;
                    default:
                        Usage();
                        return;
                }
            }

            if ((netCDFFileName == "") || (lonVarName == "") || (latVarName == "") || (dataVarName == "") || (depthVarName == "") || (missingValueAttName == "") || (scaleFactorAttName == "") || (offsetValueAttName == ""))
            {
                Usage();
                return;
            }

            if (!File.Exists(netCDFFileName)) throw new FileNotFoundException("ImportNetCDF: File {0} not found", netCDFFileName);

            ImportNetCDF(netCDFFileName, dataVarName, lonVarName, latVarName, depthVarName, missingValueAttName, scaleFactorAttName, offsetValueAttName, outputDataFileName, north, south, east, west);

            Console.WriteLine(@"success!");
        }

        static void ImportNetCDF(string netCDFFileName, string dataVarName, string lonVarName, string latVarName, string depthVarName, string missingValueAttName, string scaleFactorAttName, string offsetValueAttName, string outputDataFileName, float north, float south, float east, float west)
        {
            var myFile = new NcFile(netCDFFileName);
            int depthCount, lonIndex, depth;
            short missingValue;
            double[] depths;

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

            if (depthVarName != String.Empty)
            {
                depthCount = (int) depthVar.Dimensions[0].Size;
                depths = new double[depthCount];
                for (depth = 0; depth < depthCount; depth++) depths[depth] = depthVar.GetFloat(depth);
            }
            else
            {
                depthCount = 1;
                depths = new double[1];
            }


            var latCount = selectedLats.Length;
            var lonCount = selectedLons.Length;

            var scaleFactor = 1.0f;
            var addOffset = 0.0f;
            if (!short.TryParse(missingValueAttName, out missingValue))
            {
                if (missingValueAttName != String.Empty) missingValue = dataVar.Attributes[missingValueAttName].GetShort(0);
            }
            if (scaleFactorAttName != String.Empty) scaleFactor = dataVar.Attributes[scaleFactorAttName].GetFloat(0);
            if (offsetValueAttName != String.Empty) addOffset = dataVar.Attributes[offsetValueAttName].GetFloat(0);

            Console.WriteLine(@"Importing source file {0}...", Path.GetFileName(netCDFFileName));
            var progress = 0f;
            var progressStep = 1f/lonCount;

            var serializedOutput = new SerializedOutput();
            serializedOutput.DepthAxis.AddRange(depths.ToList()); 

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
                    var curDataPoint = new EnvironmentalDataPoint
                                       {
                                           Latitude_degrees = lat,
                                           Longitude_degrees = wrappedLon,
                                       };
                    if (depthVarName != String.Empty)
                    {
                        for (depth = 0; depth < depthCount; depth++)
                        {
                            var curValue = dataVar.GetShort(depth, latSourceIndex, lonSourceIndex);

                            if (curValue != missingValue) curDataPoint.Data.Add(((curValue) * scaleFactor) + addOffset);
                        }
                    }
                    else
                    {
                        if (dataVar is NcVarFloat)
                        {
                            curDataPoint.Data.Add(dataVar.GetFloat(latSourceIndex, lonSourceIndex));
                        }
                        else if (dataVar is NcVarShort)
                        {
                            curDataPoint.Data.Add(dataVar.GetShort(latSourceIndex, lonSourceIndex));
                        }
                    }
                    serializedOutput.DataPoints.Add(curDataPoint);
                }

                //Console.WriteLine(@"{0} % complete", (int)(progress * 100));
                progress += progressStep;
            }
            Console.WriteLine(@"Saving imported data ... ");
            serializedOutput.Save(outputDataFileName, null);
            

            Console.WriteLine(@"done");
        }

        

        static void Usage()
        {
            Console.WriteLine( // ReSharper disable LocalizableElement
                "Usage: {0} -input <netCDFFileName>\n" + "                    -data <DataVariable>\n" + "                    -lon <LongitudeVariable>\n" + "                    -lat <LatitudeVariable>\n" + "                   [-depth <DepthVariable>]\n" + "                   [-missingvalue <MissingValueAttribute>]\n" + "                   [-scalefactor <ScaleFactorAttribute>]\n" + "                   [-offset <OffsetValueAttribute>]\n" + "                   [-output <OutputFileName>]\n" +
                "                    -layertype <OutputLayerType>\n" + "                    -period <TimePeriod>\n" + "                   [-force]\n" + "\n" + "Where: <netCDFFileName> is the full path to an UNCOMPRESSED NetCDF file,\n" + "       typically provided by OAML for the GDEM-V and the DBDB-V datasets.\n" + "\n" + "       <DataVariable> is the name of the NetCDF variable that contains the\n" + "       'payload' data.\n" + "\n" +
                "       <LongitudeVariable> is the name of the NetCDF variable that contains the\n" + "       array of longitudes present in the payload data\n" + "\n" + "       <LatitudeVariable> is the name of the NetCDF variable that contains the\n" + "       array of latitudes present in the payload data\n" + "\n" + "       <DepthVariable> is the name of the NetCDF variable that contains the\n" + "       array of depths present in the payload data.  Only required for 3-D data\n" +
                "       sets.\n" + "\n" + "       <MissingValueAttribute> is the name of the NetCDF attribute of\n" + "       <DataVariable> that contains the value used to represent a 'no data'\n" + "       condition in the payload dataset. Not all data sets have this attribute.\n" + "\n" + "       <ScaleFactorAttribute> is the name of the NetCDF attribute of\n" + "       <DataVariable> that contains the value used to scale the data into\n" +
                "       its final range. Not all data sets have this attribute.\n" + "\n" + "       <OffsetValueAttribute> is the name of the NetCDF attribute of\n" + "       <DataVariable> that contains the value used to offset the scaled data\n" + "       to its final value. Not all data sets have this attribute.\n" + "\n" + "       <OutputFileName> is the name of the ESME Environment Binary (.eeb) file\n" + "       that will contain the data imported from the specified NetCDF file.\n" +
                "       If <OutputFileName> is not specified, <netCDFFileName> will be used\n" + "       with a .eeb extension in place of the source file extension.\n" + "\n" + "       <OutputLayerType> is one of a list of types supported by this utility.\n" + "       Legal values for <OutputLayerType> are as follows:\n" + "       wind, soundspeed, bathymetry, bottomtype, temperature, salinity,\n" + "       temperature_std_dev, salinity_std_dev\n" + "\n" +
                "       The value specified should match the data in the file being imported, or\n" + "       the output of this utility will be unpredictable.\n" + "\n" + "       <TimePeriod> is the time period represented by the data in the file\n" + "       Some examples of time periods would be the names of months or seasons.\n" + "\n" + "       -force will force the utility to re-import the input file if the\n" +
                "       output file already exists.  Normally in this situation, no action would\n" + "       be taken.\n" + "", "ImportNetCDF");
            // ReSharper restore LocalizableElement
        }
    }
}
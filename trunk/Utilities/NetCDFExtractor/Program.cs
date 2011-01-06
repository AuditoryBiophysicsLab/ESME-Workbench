using System;
using System.Collections.Generic;
using System.IO;
using System.Xml.Serialization;
using ESME.Environment;
using NetCDF;
using HRC.Navigation;

namespace ImportNetCDF
{
    internal class Program
    {
        static void Main(string[] args)
        {
            string lonVarName;
            string latVarName;
            string depthVarName;
            string dataVarName;
            string missingValueAttName;
            string scaleFactorAttName;
            string offsetValueAttName;
            string outputPath;
            string outputFileName;
            string outputLayerType;
            string timePeriod;
            string north;
            string south;
            string east;
            string west;
            string outputDataFileName;
            bool force = false;

            if (args.Length < 1)
            {
                Usage();
                return;
            }
            string netCDFFileName = lonVarName = latVarName = depthVarName = dataVarName = missingValueAttName = scaleFactorAttName = offsetValueAttName = outputFileName = outputLayerType = timePeriod = north= south = east = west = outputDataFileName ="";
            for (var i = 0; i < args.Length; i++)
            {//more args: north, south, east, west, outputFileName
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
                        depthVarName = args[++i];
                        break;
                    case "-data":
                        dataVarName = args[++i];
                        break;
                    case "-mv":
                    case "-miss":
                    case "-missing":
                    case "-missingvalue":
                        missingValueAttName = args[++i];
                        break;
                    case "-sf":
                    case "-scale":
                    case "-scalefactor":
                        scaleFactorAttName = args[++i];
                        break;
                    case "-offset":
                        offsetValueAttName = args[++i];
                        break;
                    case "-out":
                    case "-output":
                        outputFileName = args[++i];
                        break;
                    
                    case "-force":
                        force = true;
                        break;
                    default:
                        Usage();
                        return;
                }
            }
            if ((netCDFFileName == "") || (lonVarName == "") || (latVarName == "") || (dataVarName == "") || (dataVarName == "") || (dataVarName == "") || (timePeriod == ""))
            {
                Usage();
                return;
            }

            if (!File.Exists(netCDFFileName))
            {
                Console.WriteLine(@"File not found: {0}", netCDFFileName);
                return;
            }

            if (outputFileName == "") outputFileName = Path.GetFileNameWithoutExtension(netCDFFileName);

            if (Path.GetDirectoryName(outputFileName) != String.Empty)
            {
                if (!Directory.Exists(Path.GetDirectoryName(outputFileName)))
                {
                    Console.WriteLine(@"Output directory not found: {0}", outputFileName);
                    return;
                }
                outputPath = Path.GetDirectoryName(outputFileName);
            }
            else outputPath = Path.GetDirectoryName(netCDFFileName);

            if (Path.GetExtension(outputFileName).ToLower() != ".eeb") outputFileName = Path.GetFileNameWithoutExtension(outputFileName) + ".eeb";

            outputFileName = Path.Combine(outputPath, outputFileName);

            if ((force) || (!File.Exists(outputFileName)))
            {
                ImportNetCDF(netCDFFileName, dataVarName, lonVarName, latVarName, depthVarName, missingValueAttName, scaleFactorAttName, offsetValueAttName, outputFileName, outputDataFileName, outputLayerType, timePeriod);
            }
            else
            {
                Console.WriteLine(@"Output file {0} exists.  Skipping.\nUse -force to force an import anyway", Path.GetFileName(outputFileName));
            }
            DataFile test = DataFile.Open(outputFileName);
            Console.WriteLine(test.ToString());
        }

        static void ImportNetCDF(string netCDFFileName, string dataVarName, string lonVarName, string latVarName, string depthVarName, string missingValueAttName, string scaleFactorAttName, string offsetValueAttName, string dataFileName, string outputDataFileName,  string dataLayerName, string timePeriod)
        {
            var myFile = new NcFile(netCDFFileName);
            myFile.LoadAllData();
            NcVar lonVar,
                  latVar,
                  depthVar,
                  dataVar;
            int latCount,
                lonCount,
                depthCount;
            int lat,
                lon,
                depth,
                i;
            float scaleFactor,
                  addOffset;
            short missingValue = 0;
            DataLayer dataLayer;
            float[] lats,
                    lons,
                    depths;
            float progress,
                  progress_step;

            lonVar = myFile.Variables[lonVarName];
            latVar = myFile.Variables[latVarName];
            depthVar = myFile.Variables[depthVarName];
            dataVar = myFile.Variables[dataVarName];

            lons = new float[myFile.Variables[lonVarName].ElementCount];
            for (i = 0; i < lons.Length; i++) lons[i] = myFile.Variables[lonVarName].GetFloat(i);
            lats = new float[myFile.Variables[latVarName].ElementCount];
            for (i = 0; i < lats.Length; i++) lats[i] = myFile.Variables[latVarName].GetFloat(i);

            Console.Write(@"Initializing output file {0} ... ", Path.GetFileName(dataFileName));
            DataFile dataFile = DataFile.Create(dataFileName);
            if (depthVarName != String.Empty)
            {
                depthCount = (int) myFile.Variables[depthVarName].Dimensions[0].Size;
                depths = new float[depthCount];
                for (depth = 0; depth < depthCount; depth++) depths[depth] = myFile.Variables[depthVarName].GetFloat(depth);
                dataLayer = new DataLayer(dataLayerName, timePeriod, netCDFFileName, String.Format("Converted from {0} on {1}", netCDFFileName, DateTime.Now), new DataAxis("Latitude", lats), new DataAxis("Longitude", lons), new DataAxis("Depth", depths));
            }
            else
            {
                depthCount = 1;
                depths = new float[1];
                dataLayer = new DataLayer(dataLayerName, timePeriod, netCDFFileName, String.Format("Converted from {0} on {1}", netCDFFileName, DateTime.Now), new DataAxis("Latitude", lats), new DataAxis("Longitude", lons), null);
            }
            dataFile.Layers.Add(dataLayer);
            Console.WriteLine(@"done");

            latCount = (int) myFile.Variables[latVarName].Dimensions[0].Size;
            lonCount = (int) myFile.Variables[lonVarName].Dimensions[0].Size;

            //float[,] CurLatitude = new float[dataLayer.LatitudeAxis.Count, depthCount];

            missingValue = short.MaxValue;
            scaleFactor = 1.0f;
            addOffset = 0.0f;
            if (!short.TryParse(missingValueAttName, out missingValue))
            {
                if (missingValueAttName != String.Empty) missingValue = dataVar.Attributes[missingValueAttName].GetShort(0);
            }
            if (scaleFactorAttName != String.Empty) scaleFactor = dataVar.Attributes[scaleFactorAttName].GetFloat(0);
            if (offsetValueAttName != String.Empty) addOffset = dataVar.Attributes[offsetValueAttName].GetFloat(0);

            Console.WriteLine(@"Importing source file {0}...", Path.GetFileName(netCDFFileName));
            progress = 0f;
            progress_step = 1f/lonCount;
            var destPoint = new DataPoint(dataLayer);
            var sourceData = new float[depthCount];
            for (lon = 0; lon < dataLayer.LongitudeAxis.Length; lon++)
            {
                destPoint.ColumnIndex = lon;
                for (lat = 0; lat < dataLayer.LatitudeAxis.Length; lat++)
                {
                    destPoint.RowIndex = lat;
                    if (depthVarName != String.Empty)
                    {
                        for (depth = 0; depth < depthCount; depth++)
                        {
                            short curValue = dataVar.GetShort(depth, lat, lon);

                            if (curValue != missingValue) sourceData[depth] = ((curValue)*scaleFactor) + addOffset;
                            else sourceData[depth] = float.NaN;
                        }
                    }
                    else
                    {
                        if (dataVar is NcVarFloat) sourceData[0] = dataVar.GetFloat(lat, lon);
                        else if (dataVar is NcVarShort) sourceData[0] = dataVar.GetShort(lat, lon);
                    }
                    destPoint.Data = sourceData;
                    var outputData = new SerializedOutput()
                                 {};
                    OutputToDataFile(outputData, outputDataFileName);
                    //todo: output sourceData to output file here. one temperature file, one sal , one speed (all 3D data/lat;lon;[depth,value])
                }
                Console.Write(@"{0} % complete              \r", (int) (progress*100));
                progress += progress_step;
            }
            Console.Write(@"Saving imported data ... ");
            dataFile.Close();
            Console.WriteLine(@"done");
        }

        static void OutputToDataFile(SerializedOutput data, string outputDataFileName)
        {
            var serializer = new XmlSerializer(typeof(SerializedOutput));
            TextWriter writer = new StreamWriter(outputDataFileName);
            serializer.Serialize(writer,data);
        }

        internal class SerializedOutput//:SerializableData 
        {
            //other header things here, before DataPoints.
            public List<DataPointg> DataPoints { get; set; }
        }

        internal class DataPointg
        {
            public EarthCoordinate EarthCoordinate { set; get; }
            public List<DepthValuePair> Data { get; set; }
        }

        internal class DepthValuePair
        {
            public float Depth { get; set; }
            public float Value { get; set; }
        }
        static void Usage()
        {
            Console.WriteLine(
// ReSharper disable LocalizableElement
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
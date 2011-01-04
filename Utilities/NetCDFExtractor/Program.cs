using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using ESME.Environment;
using NetCDF;

namespace ImportNetCDF
{
    class Program
    {
        static void Main(string[] args)
        {
            string NetCDFileName, LonVarName, LatVarName, DepthVarName, DataVarName, 
                MissingValueAttName, ScaleFactorAttName, OffsetValueAttName,
                OutputPath, OutputFileName, OutputLayerType, TimePeriod;
            bool force = false;

            if (args.Length < 1)
            {
                Usage();
                return;
            }
            NetCDFileName = LonVarName = LatVarName = DepthVarName = DataVarName = 
                MissingValueAttName = ScaleFactorAttName = OffsetValueAttName = OutputFileName = OutputLayerType = TimePeriod = "";
            for (int i = 0; i < args.Length; i++)
            {
                switch (args[i])
                {
                    case "-in":
                    case "-input":
                        NetCDFileName = args[++i];
                        break;
                    case "-lon":
                    case "-longitude":
                        LonVarName = args[++i];
                        break;
                    case "-lat":
                    case "-latitude":
                        LatVarName = args[++i];
                        break;
                    case "-dep":
                    case "-depth":
                        DepthVarName = args[++i];
                        break;
                    case "-data":
                        DataVarName = args[++i];
                        break;
                    case "-mv":
                    case "-miss":
                    case "-missing":
                    case "-missingvalue":
                        MissingValueAttName = args[++i];
                        break;
                    case "-sf":
                    case "-scale":
                    case "-scalefactor":
                        ScaleFactorAttName = args[++i];
                        break;
                    case "-offset":
                        OffsetValueAttName = args[++i];
                        break;
                    case "-out":
                    case "-output":
                        OutputFileName = args[++i];
                        break;
                    case "-type":
                    case "-layertype":
                        OutputLayerType = args[++i].ToLower();
                        switch (OutputLayerType)
                        {
                            case "wind":
                            case "soundspeed":
                            case "bathymetry":
                            case "bottomtype":
                            case "temperature":
                            case "salinity":
                            case "temperature_std_dev":
                            case "salinity_std_dev":
                                break;
                            default:
                                Usage();
                                return;
                        }
                        break;
                    case "-time":
                    case "-period":
                        TimePeriod = args[++i];
                        break;
                    case "-force":
                        force = true;
                        break;
                    default:
                        Usage();
                        return;
                }
            }
            if ((NetCDFileName == "") || (LonVarName == "") || (LatVarName == "") || 
                (DataVarName == "") || (DataVarName == "") || (DataVarName == "") || (TimePeriod == ""))
            {
                Usage();
                return;
            }

            if (!File.Exists(NetCDFileName))
            {
                Console.WriteLine("File not found: {0}", NetCDFileName);
                return;
            }

            if (OutputFileName == "")
                OutputFileName = Path.GetFileNameWithoutExtension(NetCDFileName);
            
            if (Path.GetDirectoryName(OutputFileName) != String.Empty)
            {
                if (!Directory.Exists(Path.GetDirectoryName(OutputFileName)))
                {
                    Console.WriteLine("Output directory not found: {0}", OutputFileName);
                    return;
                }
                OutputPath = Path.GetDirectoryName(OutputFileName);
            }
            else
                OutputPath = Path.GetDirectoryName(NetCDFileName);

            if (Path.GetExtension(OutputFileName).ToLower() != ".eeb")
                OutputFileName = Path.GetFileNameWithoutExtension(OutputFileName) + ".eeb";

            OutputFileName = Path.Combine(OutputPath, OutputFileName);

            if ((force) || (!File.Exists(OutputFileName)))
            {
                ImportNetCDF(NetCDFileName, DataVarName, LonVarName, LatVarName, DepthVarName,
                    MissingValueAttName, ScaleFactorAttName, OffsetValueAttName,
                    OutputFileName, OutputLayerType, TimePeriod);
            }
            else
            {
                Console.WriteLine("Output file {0} exists.  Skipping.\nUse -force to force an import anyway", Path.GetFileName(OutputFileName));
            }
            DataFile test = DataFile.Open(OutputFileName);
            Console.WriteLine(test.ToString());
        }

        static void ImportNetCDF(string NetCDFileName, string DataVarName, string LonVarName, string LatVarName, string DepthVarName,
                string MissingValueAttName, string ScaleFactorAttName, string OffsetValueAttName, 
                string DataFileName, string DataLayerName, string TimePeriod)
        {
            NcFile myFile = new NcFile(NetCDFileName);
            myFile.LoadAllData();
            NcVar lonVar, latVar, depthVar, dataVar;
            int latCount, lonCount, depthCount;
            int lat, lon, depth, i;
            float scaleFactor, addOffset;
            short missingValue = 0;
            DataLayer dataLayer;
            float[] lats, lons, depths;
            float progress, progress_step;

            lonVar = myFile.Variables[LonVarName];
            latVar = myFile.Variables[LatVarName];
            depthVar = myFile.Variables[DepthVarName];
            dataVar = myFile.Variables[DataVarName];

            lons = new float[myFile.Variables[LonVarName].ElementCount];
            for (i = 0; i < lons.Length; i++)
                lons[i] = myFile.Variables[LonVarName].GetFloat(i);
            lats = new float[myFile.Variables[LatVarName].ElementCount];
            for (i = 0; i < lats.Length; i++)
                lats[i] = myFile.Variables[LatVarName].GetFloat(i);

            Console.Write("Initializing output file {0} ... ", Path.GetFileName(DataFileName));
            DataFile dataFile = DataFile.Create(DataFileName);
            if (DepthVarName != String.Empty)
            {
                depthCount = (int)myFile.Variables[DepthVarName].Dimensions[0].Size;
                depths = new float[depthCount];
                for (depth = 0; depth < depthCount; depth++)
                    depths[depth] = myFile.Variables[DepthVarName].GetFloat(depth);
                dataLayer = new DataLayer(DataLayerName, TimePeriod, NetCDFileName,
                    String.Format("Converted from {0} on {1}", NetCDFileName, DateTime.Now.ToString()),
                    new DataAxis("Latitude", lats), new DataAxis("Longitude", lons), new DataAxis("Depth", depths));
            }
            else
            {
                depthCount = 1;
                depths = new float[1];
                dataLayer = new DataLayer(DataLayerName, TimePeriod, NetCDFileName,
                    String.Format("Converted from {0} on {1}", NetCDFileName, DateTime.Now.ToString()),
                    new DataAxis("Latitude", lats), new DataAxis("Longitude", lons), null);
            }
            dataFile.Layers.Add(dataLayer);
            Console.WriteLine("done");

            latCount = (int)myFile.Variables[LatVarName].Dimensions[0].Size;
            lonCount = (int)myFile.Variables[LonVarName].Dimensions[0].Size;

            //float[,] CurLatitude = new float[dataLayer.LatitudeAxis.Count, depthCount];

            missingValue = short.MaxValue;
            scaleFactor = 1.0f;
            addOffset = 0.0f;
            if (!short.TryParse(MissingValueAttName, out missingValue))
            {
                if (MissingValueAttName != String.Empty)
                    missingValue = dataVar.Attributes[MissingValueAttName].GetShort(0);
            }
            if (ScaleFactorAttName != String.Empty)
                scaleFactor = dataVar.Attributes[ScaleFactorAttName].GetFloat(0);
            if (OffsetValueAttName != String.Empty)
                addOffset = dataVar.Attributes[OffsetValueAttName].GetFloat(0);

            Console.WriteLine("Importing source file {0}...", Path.GetFileName(NetCDFileName));
            progress = 0f;
            progress_step = 1f / lonCount;
            DataPoint destPoint = new DataPoint(dataLayer);
            float[] sourceData = new float[depthCount];
            for (lon = 0; lon < dataLayer.LongitudeAxis.Length; lon++)
            {
                destPoint.ColumnIndex = lon;
                for (lat = 0; lat < dataLayer.LatitudeAxis.Length; lat++)
                {
                    destPoint.RowIndex = lat;
                    if (DepthVarName != String.Empty)
                    {
                        for (depth = 0; depth < depthCount; depth++)
                        {
                            short curValue = dataVar.GetShort(depth, lat, lon);

                            if (curValue != missingValue)
                                sourceData[depth] = (((float)curValue) * scaleFactor) + addOffset;
                            else
                                sourceData[depth] = float.NaN;
                        }
                    }
                    else
                    {
                        if (dataVar is NcVarFloat)
                            sourceData[0] = dataVar.GetFloat(lat, lon);
                        else if (dataVar is NcVarShort)
                            sourceData[0] = dataVar.GetShort(lat, lon);
                    }
                    destPoint.Data = sourceData;
                }
                Console.Write("{0} % complete              \r", (int)(progress * 100));
                progress += progress_step;
            }
            Console.Write("Saving imported data ... ");
            dataFile.Close();
            Console.WriteLine("done");
        }

        static void Usage()
        {
            Console.WriteLine(
                "Usage: {0} -input <NetCDFileName>\n" + 
                "                    -data <DataVariable>\n" + 
                "                    -lon <LongitudeVariable>\n" + 
                "                    -lat <LatitudeVariable>\n" + 
                "                   [-depth <DepthVariable>]\n" + 
                "                   [-missingvalue <MissingValueAttribute>]\n" + 
                "                   [-scalefactor <ScaleFactorAttribute>]\n" + 
                "                   [-offset <OffsetValueAttribute>]\n" +
                "                   [-output <OutputFileName>]\n" +
                "                    -layertype <OutputLayerType>\n" +
                "                    -period <TimePeriod>\n" +
                "                   [-force]\n" +
                "\n" +
                "Where: <NetCDFileName> is the full path to an UNCOMPRESSED NetCDF file,\n" + 
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
                "       array of depths present in the payload data.  Only required for 3-D data\n" +
                "       sets.\n" +
                "\n" +
                "       <MissingValueAttribute> is the name of the NetCDF attribute of\n" +
                "       <DataVariable> that contains the value used to represent a 'no data'\n" +
                "       condition in the payload dataset. Not all data sets have this attribute.\n" +
                "\n" +
                "       <ScaleFactorAttribute> is the name of the NetCDF attribute of\n" +
                "       <DataVariable> that contains the value used to scale the data into\n" +
                "       its final range. Not all data sets have this attribute.\n" +
                "\n" +
                "       <OffsetValueAttribute> is the name of the NetCDF attribute of\n" +
                "       <DataVariable> that contains the value used to offset the scaled data\n" +
                "       to its final value. Not all data sets have this attribute.\n" +
                "\n" +
                "       <OutputFileName> is the name of the ESME Environment Binary (.eeb) file\n" +
                "       that will contain the data imported from the specified NetCDF file.\n" +
                "       If <OutputFileName> is not specified, <NetCDFileName> will be used\n" +
                "       with a .eeb extension in place of the source file extension.\n" +
                "\n" +
                "       <OutputLayerType> is one of a list of types supported by this utility.\n" +
                "       Legal values for <OutputLayerType> are as follows:\n" +
                "       wind, soundspeed, bathymetry, bottomtype, temperature, salinity,\n" +
                "       temperature_std_dev, salinity_std_dev\n" +
                "\n" +
                "       The value specified should match the data in the file being imported, or\n" +
                "       the output of this utility will be unpredictable.\n" +
                "\n" +
                "       <TimePeriod> is the time period represented by the data in the file\n" +
                "       Some examples of time periods would be the names of months or seasons.\n" +
                "\n" +
                "       -force will force the utility to re-import the input file if the\n" +
                "       output file already exists.  Normally in this situation, no action would\n" +
                "       be taken.\n" +
                ""
                , "ImportNetCDF"
                );
        }
    }
}

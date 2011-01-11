using System;
using System.Collections.Generic;
using System.IO;
using System.Xml.Serialization;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment.NAVO
{
    class GDEM : NAVODataSource
    {
        public override void ExtractArea(string filename, double north, double south, double east, double west)
        {
            CommandArgs = string.Format("-in {0} -lon -lat -north -south -east -west -dep -sf -offset -out -dataout -force ",DatabasePath);
            var result = Execute();
            var serializer = new XmlSerializer(typeof (SerializedOutput));
            var reader = new StreamReader(filename);
            var parsedOutput = (SerializedOutput)serializer.Deserialize(reader);
            
            //extract temperature

            //extract salinity 

            //combine.

            //output.

            var dictionary = new Dictionary<string, List<DepthValuePair>>();
            var valuesLength = parsedOutput.DataPoints[0].Data.Count;
            var tempvalues = new float[valuesLength, valuesLength];
            
            foreach (var point in parsedOutput.DataPoints)
            {
                    if (point.Data != null) dictionary.Add(string.Format("{0:#.00000},{1:#.00000}", point.EarthCoordinate.Latitude_degrees, point.EarthCoordinate.Longitude_degrees), point.Data);
            }
            for (var latIndex = 0; latIndex < parsedOutput.DataPoints.Count; latIndex++ )
            {
                var lat = parsedOutput.DataPoints[latIndex].EarthCoordinate.Latitude_degrees;
                for (var lonIndex = 0; lonIndex < parsedOutput.DataPoints.Count; lonIndex++)
                {
                    var lon = parsedOutput.DataPoints[latIndex].EarthCoordinate.Longitude_degrees;
                    var key = string.Format("{0:#.00000},{1:#.00000}", lat, lon);
                    
                    
                }
            }

            //tempvalues must contain ssp not temp or sal.
            ExtractedArea = new Environment2DData(north, south, east, west, 1,tempvalues,0,0 );
            
        }
        public override bool ValidateDataSource() { return false; }
    }

}

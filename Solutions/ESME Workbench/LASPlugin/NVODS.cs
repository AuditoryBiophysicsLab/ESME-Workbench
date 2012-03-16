using System;
using System.ComponentModel.Composition;
using System.Globalization;
using System.Net;
using System.Text;
using System.Xml;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Plugins;
using HRC.Navigation;
using HRC.Utility;



namespace LASPlugin
{
    [Serializable]
    [PartCreationPolicy(CreationPolicy.Shared)]
    [EnvironmentDataSource(EnvironmentDataType = EnvironmentDataType.Sediment,
                           Name = "NVODS",
                           Description = "National Virtual Oceanographic Data System")]
    public sealed class NVODS:EnvironmentalDataSourcePluginBase<SoundSpeed>
    {
        private readonly string _url = @"http://ferret.pmel.noaa.gov/NVODS/";
        public override SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null)
        {
            throw new NotImplementedException();
        }


    }
    //ported from http://ferret.pmel.noaa.gov/FERRET_17sep07/LAS/FAQ/ls.pl
    internal class LASQUery
    {
        private readonly string _reqURL;
        private readonly string _dataset;
        private readonly string _variable;

        public LASQUery(string requestedURL, string dataset, string variable, string seperator = null)
        {
            _reqURL = requestedURL;// 
            _dataset = dataset;
            _variable = variable;
        }
        private void IssueRequest()
        {
             //WebRequestMethods.Http.Get
            
        }
        private void GetXMLArgs(XmlWriter writer)
        {
            writer.WriteStartElement("ran");
                var r = new Random();
                writer.WriteString(r.NextDouble().ToString(CultureInfo.InvariantCulture));
            writer.WriteEndElement();
            if(false)
            {
                writer.WriteStartElement("modifiers");
                writer.WriteString("long");
                writer.WriteEndElement();
            }
            writer.WriteStartElement("dataset");
            writer.WriteString(_dataset);




        }
        private void GenXML()
        {
            var output = new StringBuilder();
            //var args = GetXMLArgs();
            using (var writer = XmlWriter.Create(output))
            {
                writer.WriteStartDocument();
                
                writer.WriteEndDocument();
            }

        }

        void Main()
        {
            
        }

    }

    //ported from http://ferret.pmel.noaa.gov/FERRET_17sep07/LAS/FAQ/lasget.pl
    internal class LASDataRequest
    {

    }
}

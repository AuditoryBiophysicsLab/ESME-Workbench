using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using System.Xml;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Locations;
using ESME.Plugins;
using ESME.Views.Locations;
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

        public override IEnumerable<EnvironmentalDataSet> SelectedDataSets
        {
            get { throw new NotImplementedException(); }
        }
    }
    //ported from http://ferret.pmel.noaa.gov/FERRET_17sep07/LAS/FAQ/ls.pl
    internal class LASQUery
    {
        private readonly string _dataset;
        private readonly string _variable;
        private readonly string _separators;
        private readonly Uri _url;

        public LASQUery(string requestedURL, string dataset, string variable, string separator = null)
        {
            _dataset = dataset;
            _variable = variable;
            if(separator != null) _separators = separator;
            if(_separators.Contains("&")||_separators.Contains("<") || _separators.Contains(">")) throw new Exception("invalid separator string!");
            if (!Uri.TryCreate(requestedURL, UriKind.RelativeOrAbsolute, out _url)) throw new Exception("URL invalid!");
        }
        public string IssueRequest()
        {
            var xmlQuery = GenXML();
            var client = new WebClient();
            var data = client.OpenWrite(_url);
            var response = client.OpenRead(_url);
            var writer = new StreamWriter(data);
            writer.Write(xmlQuery);

            if (response != null)
            {
                var reader = new StreamReader(response);
                var reply = reader.ReadToEnd();
                reader.Dispose();
                writer.Dispose();
                response.Close();
                data.Close();
                return reply;
            }
            return null;
        }
        private void GetXMLArgs(XmlWriter writer)
        {
            writer.WriteStartElement("ran");
                var r = new Random();
                writer.WriteString(r.NextDouble().ToString(CultureInfo.InvariantCulture));
            writer.WriteEndElement();

            if(_separators !=null)
            {
                writer.WriteStartElement("modifiers");
                writer.WriteString("long");
                writer.WriteEndElement();
            }

            writer.WriteStartElement("dataset");
            writer.WriteString(_dataset);
            writer.WriteEndElement();

            writer.WriteStartElement("variable");
            writer.WriteString(_variable);
            writer.WriteEndElement();

            writer.WriteStartElement("separator");
            writer.WriteString(_separators);
            writer.WriteEndElement();
        }
        private string GenXML()
        {
            var output = new StringBuilder();
            
            using (var writer = XmlWriter.Create(output))
            {
                writer.WriteStartDocument();
                writer.WriteStartElement("lasRequest");
                writer.WriteElementString("href",null,"file:las.xml");
                writer.WriteStartElement("link");
                writer.WriteElementString("match", null, "lasdata/operations/ls");


                writer.WriteStartElement("properties");
                writer.WriteStartElement("ls");
                GetXMLArgs(writer);
                writer.WriteEndElement();
                writer.WriteEndElement();
                writer.WriteStartElement("args");
                writer.WriteEndElement();
                writer.WriteEndElement();
                writer.WriteEndDocument();
            }
            return output.ToString();
        }
    }

    //ported from http://ferret.pmel.noaa.gov/FERRET_17sep07/LAS/FAQ/lasget.pl
    internal class LASDataRequest
    {

    }
}

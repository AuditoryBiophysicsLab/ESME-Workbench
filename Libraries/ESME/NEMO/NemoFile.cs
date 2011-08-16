using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;

namespace ESME.NEMO
{
    public class NemoFile : NemoBase
    {
        readonly XmlDocument _xmlDocument;

        public NemoFile(string fileName, string nemoDataDirectory)
        {
            try
            {
                FileName = fileName;
                _xmlDocument = new XmlDocument();
                _xmlDocument.Load(FileName);
                Scenario = new NemoScenario(_xmlDocument["Scenario"], nemoDataDirectory);
            }
            catch (Exception e)
            {
                throw new FileFormatException(string.Format("Error opening NEMO file \"{0}\"", FileName), e);
            }
        }

        public override IEnumerable<KeyValuePair<string, string>> Properties
        {
            get
            {
                yield return new KeyValuePair<string, string>("File name", FileName);
            }
        }

        public string FileName { get; private set; }

        public NemoScenario Scenario { get; private set; }

        public override string ToString() { return Path.GetFileName(FileName); }
    }
}
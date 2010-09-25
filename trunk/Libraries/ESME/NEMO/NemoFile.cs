using System;
using System.IO;
using System.Xml;

namespace ESME.NEMO
{
    public class NemoFile : NemoBase
    {
        private readonly string _fileName;
        private readonly XmlDocument _nemoDocument;

        public NemoFile(string fileName, string nemoDataDirectory)
        {
            try
            {
                _fileName = fileName;
                _nemoDocument = new XmlDocument();
                _nemoDocument.Load(fileName);
                Scenario = new NemoScenario(_nemoDocument["Scenario"], nemoDataDirectory);
            }
            catch (Exception e)
            {
                throw new FileFormatException(string.Format("Error opening NEMO file \"{0}\"", fileName), e);
            }
        }

        public NemoScenario Scenario { get; private set; }

        public override string ToString()
        {
            return Path.GetFileName(_fileName);
        }
    }
}
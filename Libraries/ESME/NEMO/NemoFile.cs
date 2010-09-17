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
            _fileName = fileName;
            _nemoDocument = new XmlDocument();
            _nemoDocument.Load(fileName);
            Scenario = new NemoScenario(_nemoDocument["Scenario"], nemoDataDirectory);
        }

        public NemoScenario Scenario { get; private set; }

        public override string ToString()
        {
            return Path.GetFileName(_fileName);
        }
    }
}
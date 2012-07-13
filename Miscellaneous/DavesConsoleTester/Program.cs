using System;
using System.Xml;

namespace DavesConsoleTester
{
    class Program
    {
        static void Main(string[] args)
        {
            var reader = new XmlTextReader("http://esme.bu.edu/data/version.xml");
            while (reader.Read())
            {
                switch (reader.NodeType)
                {
                    case XmlNodeType.Element:
                        switch (reader.Name)
                        {
                            case "Version":
                                reader.Read();
                                if (reader.NodeType == XmlNodeType.Text)
                                {
                                    Console.WriteLine("Version: {0}", reader.Value);
                                    var v = new Version(reader.Value);
                                }
                                break;
                        }
                        break;
                }
            }
            reader.Close();
        }
    }
}

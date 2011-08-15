namespace ESME.NewNEMO
{
    public class NewNEMOTest
    {
        public NewNEMOTest(string nemoFilename)
        {
            var str = new System.IO.StreamReader(nemoFilename);
            var xSerializer = new System.Xml.Serialization.XmlSerializer(typeof(Scenario));
            var result = (Scenario)xSerializer.Deserialize(str);
            str.Close();
        }
    }
}

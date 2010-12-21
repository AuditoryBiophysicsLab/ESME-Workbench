namespace ESME.Environment.NAVO
{
    class GDEM : NAVODataSource
    {
        public override void ExtractArea(string filename, double north, double south, double east, double west) { }
        public override bool ValidateDataSource() { return false; }
    }
}

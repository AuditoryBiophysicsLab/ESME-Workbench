namespace DavesWPFTester.AxisLabeling.Language
{
    public class Range
    {
        public Range(double min, double max)
        {
            Min = min;
            Max = max;
        }

        public double Min { get; set; }
        public double Max { get; set; }
        public double Size { get { return Max - Min; } }

        public double ValueToRange(double value) { return (value - Min) / Size; }

        public double RangeToValue(double range) { return (range * Size) + Min; }

        public static Range Identity = new Range(0, 1);

        public Range Expand(double amount) { return new Range(Min - amount, Max + amount); }
    };
}
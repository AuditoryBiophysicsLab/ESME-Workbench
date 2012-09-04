using System;
using System.Linq;
using DavesWPFTester.AxisLabeling.Language.Types;

namespace DavesWPFTester.AxisLabeling.Layout.AxisLabelers
{
    // Categorical labeling code
    class CategoricalAxisLabeler : AxisLabeler
    {
        public override Axis Generate(AxisLabelerOptions options, double density)
        {
            var f = (Factor)options.Symbol;

            var labels = f.AllLevels.Select(level => new Tuple<double, string>(level.LevelIndex, level.ToString())).ToList();

            var result = options.DefaultAxis();
            result.Labels = labels;

            return result;
        }
    }
}
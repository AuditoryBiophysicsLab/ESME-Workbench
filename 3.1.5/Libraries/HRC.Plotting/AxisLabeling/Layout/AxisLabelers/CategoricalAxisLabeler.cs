using System.Linq;
using HRC.Plotting.AxisLabeling.Language.Types;

namespace HRC.Plotting.AxisLabeling.Layout.AxisLabelers
{
    // Categorical labeling code
    class CategoricalAxisLabeler : AxisLabeler
    {
        public override Axis Generate(AxisLabelerOptions options, double density)
        {
            var f = (Factor)options.Symbol;

            var labels = f.AllLevels.Select(level => new AxisLabel(level.LevelIndex, level.ToString())).ToList();

            var result = options.DefaultAxis();
            result.Labels = labels;

            return result;
        }
    }
}
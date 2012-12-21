using System;
using System.Collections.Generic;
using HRC.Plotting.AxisLabeling.Layout.AxisLabelers;

namespace HRC.Plotting.AxisLabeling.Layout.Formatters
{
    abstract class Formatter
    {
        public abstract Axis Format(List<Axis> list, List<Format> formats, AxisLabelerOptions options, Func<Axis, double> ScoreAxis, double bestScore = double.NegativeInfinity);
    }
}
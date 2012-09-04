using System;
using System.Collections.Generic;
using DavesWPFTester.AxisLabeling.Layout.AxisLabelers;

namespace DavesWPFTester.AxisLabeling.Layout.Formatters
{
    abstract class Formatter
    {
        public abstract Axis Format(List<Axis> list, List<Format> formats, AxisLabelerOptions options, Func<Axis, double> ScoreAxis, double bestScore = double.NegativeInfinity);
    }
}
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;

namespace ESME.Views.Controls
{

    #region StepFunction class

    internal class StepFunction : List<Point>
    {
        public StepFunction(double startX, double endX, int numSteps, Func<double, double> mappingFunction) 
        { 
            for (var x = startX; x <= endX; x += (endX - startX)/numSteps) 
                Add(new Point(x, mappingFunction(x))); 
        }

        Point? FindY(double currentY)
        {
            for (var i = 0; i < this.Count() - 1; i++) 
                if ((this[i].Y <= currentY) && (currentY > this[i + 1].Y)) 
                    return this[i];
            return null;
        }

        public double StepForward(double currentY)
        {
            var curStep = FindY(currentY);

            if (curStep == null) return this[Count - 1].Y;

            var nextIndex = IndexOf(curStep.Value) + 1;

            if (nextIndex >= Count) return this[Count - 1].Y;
            return currentY + this[nextIndex].Y - curStep.Value.Y;
        }

        public double StepBack(double currentY)
        {
            var curStep = FindY(currentY);

            if (curStep == null) return this[0].Y;

            var prevIndex = IndexOf(curStep.Value) - 1;

            if (prevIndex < 0) return this[0].Y;
            return currentY + this[prevIndex].Y - curStep.Value.Y;
        }
    }

    #endregion
}
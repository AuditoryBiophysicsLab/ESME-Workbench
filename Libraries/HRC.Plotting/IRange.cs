using System;

namespace HRC.Plotting
{
    public interface IRange : INotifyRangeChanged, IEquatable<IRange>, IObservable<IRange>
    {
        double Min { get; }
        double Max { get; }
        double Size { get; }
        bool IsEmpty { get; }
        bool Contains(IRange otherRange);
        bool Contains(double value);
    }
}
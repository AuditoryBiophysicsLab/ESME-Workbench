using System;

namespace HRC.Plotting
{
    public interface IRange : INotifyRangeChanged, IEquatable<IRange>, IObservable<IRange>
    {
        double Min { get; }
        double Max { get; }
        double Value { get; }
        bool IsEmpty { get; }
    }
}
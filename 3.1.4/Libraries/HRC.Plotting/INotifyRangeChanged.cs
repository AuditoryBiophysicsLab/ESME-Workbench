using System;

namespace HRC.Plotting
{
    public interface INotifyRangeChanged
    {
        event EventHandler<NotifyRangeChangedEventArgs> RangeChanged;
    }
}
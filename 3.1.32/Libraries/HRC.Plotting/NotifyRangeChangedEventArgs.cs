using System;

namespace HRC.Plotting
{
    public class NotifyRangeChangedEventArgs : EventArgs
    {
        public NotifyRangeChangedEventArgs(IRange oldRange) { OldRange = oldRange; }
        public IRange OldRange { get; private set; }
    }
}
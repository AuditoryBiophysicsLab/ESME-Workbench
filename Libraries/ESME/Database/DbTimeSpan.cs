using System;
using System.ComponentModel.DataAnnotations;

namespace ESME.Database
{
    [ComplexType]
    public class DbTimeSpan
    {
        public DbTimeSpan() {}
        public DbTimeSpan(DateTime dateTime) { Ticks = dateTime.Ticks; }
        public DbTimeSpan(TimeSpan timeSpan) { Ticks = timeSpan.Ticks; }
        public static implicit operator DbTimeSpan(TimeSpan timeSpan) { return new DbTimeSpan(timeSpan); }
        public static implicit operator TimeSpan(DbTimeSpan dbTimeSpan) { return new TimeSpan(dbTimeSpan.Ticks); }
        public long Ticks { get; set; }

        [NotMapped] 
        public TimeSpan TimeSpan
        {
            get { return new TimeSpan(Ticks); }
            set { Ticks = value.Ticks; }
        }
    }
}
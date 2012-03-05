using System;
using System.ComponentModel.DataAnnotations;
using System.Data.Entity;

namespace ESME.Database
{
    [ComplexType]
    public class DbTimeSpan
    {
        public DbTimeSpan(DateTime dateTime) { Ticks = dateTime.Ticks; }
        public DbTimeSpan(TimeSpan timeSpan) { Ticks = timeSpan.Ticks; }
        public static implicit operator DbTimeSpan(TimeSpan timeSpan) { return new DbTimeSpan(timeSpan); }
        public static implicit operator TimeSpan(DbTimeSpan dbTimeSpan) { return new TimeSpan(dbTimeSpan.Ticks); }
        public long Ticks { get; set; }

        public TimeSpan TimeSpan
        {
            get { return new TimeSpan(Ticks); }
            set { Ticks = value.Ticks; }
        }

        internal static void ModelInitialization(DbModelBuilder modelBuilder)
        {
            modelBuilder.ComplexType<DbTimeSpan>()
                .Ignore(p => p.TimeSpan);
        }
    }
}
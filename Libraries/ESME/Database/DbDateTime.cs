using System;
using System.ComponentModel.DataAnnotations;
using System.Data.Entity;

namespace ESME.Database
{
    [ComplexType]
    public class DbDateTime
    {
        public DbDateTime(DateTime dateTime) { Ticks = dateTime.Ticks; }
        public static implicit operator DbDateTime(DateTime dateTime) { return new DbDateTime(dateTime); }
        public static implicit operator DateTime(DbDateTime dbDateTime) { return new DateTime(dbDateTime.Ticks); }
        public long Ticks { get; set; }

        public DateTime DateTime
        {
            get { return new DateTime(Ticks); }
            set { Ticks = value.Ticks; }
        }

        internal static void ModelInitialization(DbModelBuilder modelBuilder)
        {
            modelBuilder.ComplexType<DbDateTime>()
                .Ignore(p => p.DateTime);
        }
    }
}
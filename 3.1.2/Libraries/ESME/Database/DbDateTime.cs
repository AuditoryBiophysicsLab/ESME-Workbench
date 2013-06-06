using System;
using System.ComponentModel.DataAnnotations;

namespace ESME.Database
{
    [ComplexType]
    public class DbDateTime
    {
        public DbDateTime() { }
        public DbDateTime(DateTime dateTime) { Ticks = dateTime.Ticks; }
        public static implicit operator DbDateTime(DateTime dateTime) { return new DbDateTime(dateTime); }
        public static implicit operator DateTime(DbDateTime dbDateTime) { return new DateTime(dbDateTime.Ticks); }
        public long Ticks { get; set; }
    }
}
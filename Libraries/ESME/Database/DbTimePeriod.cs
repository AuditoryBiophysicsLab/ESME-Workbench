using System.ComponentModel.DataAnnotations;
using ESME.Environment;

namespace ESME.Database
{
    [ComplexType]
    public class DbTimePeriod
    {
        public DbTimePeriod() { }
        public DbTimePeriod(TimePeriod timePeriod) { TimePeriodAsByte = (byte)timePeriod; }
        public static implicit operator DbTimePeriod(TimePeriod timePeriod) { return new DbTimePeriod(timePeriod); }
        public static implicit operator TimePeriod(DbTimePeriod dbTimePeriod) { return (TimePeriod)dbTimePeriod.TimePeriodAsByte; }
        public byte TimePeriodAsByte { get; set; }

        [NotMapped]
        public TimePeriod TimePeriod
        {
            get { return (TimePeriod)TimePeriodAsByte; }
            set { TimePeriodAsByte = (byte)value; }
        }
    }
}
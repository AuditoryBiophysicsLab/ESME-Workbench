using System;

namespace ESME.Database
{
    public class DbWhoWhenWhere
    {
        public DbWhoWhenWhere() {}
        public DbWhoWhenWhere(bool useCurrent)
        {
            if (!useCurrent) return;
            When = DateTime.Now;
            Where = System.Environment.MachineName;
            Who = System.Environment.UserName;
        }
        public string Who { get; set; }
        public DbDateTime When { get; set; }
        public string Where { get; set; }
        public override string ToString() { return string.Format("{0} at {1} on {2}", Who, (DateTime)When, Where); }
    }
}

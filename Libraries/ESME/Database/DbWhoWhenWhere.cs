using System;
using System.ComponentModel.DataAnnotations;

namespace ESME.Database
{
    [ComplexType]
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
    }
}

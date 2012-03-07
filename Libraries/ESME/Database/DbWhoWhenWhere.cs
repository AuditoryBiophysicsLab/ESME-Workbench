using System.ComponentModel.DataAnnotations;

namespace ESME.Database
{
    [ComplexType]
    public class DbWhoWhenWhere
    {
        public string Who { get; set; }
        public DbDateTime When { get; set; }
        public string Where { get; set; }
    }
}

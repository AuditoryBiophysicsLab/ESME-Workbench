using System.ComponentModel.DataAnnotations;
using ESME.Scenarios;
using HRC.Navigation;

namespace ESME.Simulator
{
    public class Actor
    {
        [Key]
        public int ID { get; set; }
        public Platform Platform { get; set; }
        public AnimatLocation AnimatLocation { get; set; }
    }
}
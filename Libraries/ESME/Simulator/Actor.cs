using System.ComponentModel.DataAnnotations;
using ESME.Locations;
using ESME.Scenarios;

namespace ESME.Simulator
{
    public class Actor
    {
        [Key]
        public int ActorID { get; set; }
        public Platform Platform { get; set; }
        public AnimatLocation Animat { get; set; }
    }
}
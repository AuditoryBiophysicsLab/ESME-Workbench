using System.ComponentModel.DataAnnotations;
using ESME.Scenarios;

namespace ESME.Simulator
{
    public class Actor
    {
        [Key]
        public int ID { get; set; }
        public Platform Platform { get; set; }
        public ScenarioSpecies Species { get; set; }
    }
}
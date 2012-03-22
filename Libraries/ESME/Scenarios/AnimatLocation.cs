using System.ComponentModel.DataAnnotations;
using ESME.Database;

namespace ESME.Scenarios
{
    public class AnimatLocation
    {
        [Key]
        public int ID { get; set; }
        public DbGeo Geo { get; set; }
        public float Depth { get; set; }
        public virtual ScenarioSpecies ScenarioSpecies { get; set; }
    }
}
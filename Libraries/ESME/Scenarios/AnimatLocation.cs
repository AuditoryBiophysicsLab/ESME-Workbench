using ESME.Database;

namespace ESME.Scenarios
{
    public class AnimatLocation
    {
        public int AnimatLocationID { get; set; }
        public DbGeo Geo { get; set; }
        public float Depth { get; set; }
        public virtual ScenarioSpecies ScenarioSpecies { get; set; }
    }
}
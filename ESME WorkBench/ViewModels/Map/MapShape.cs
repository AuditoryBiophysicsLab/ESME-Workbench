using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Map
{
    // This is our MapShape and it will be the single unit in our layer.

    internal class MapShape
    {
        public MapShape() : this(new Feature()) { }

        // Let's use this as a handy constructor if you already have
        // a feature or want to create one inline.
        public MapShape(Feature feature)
        {
            Feature = feature;
            ZoomLevels = new ZoomLevelSet();
        }

        //  This is the feature property, pretty simple.
        public Feature Feature { get; set; }

        // This is the Zoom Level Set.  This high level object has all of
        // the logic in it for zoom levels, drawing and everything.
        public ZoomLevelSet ZoomLevels { get; set; }
    }
}
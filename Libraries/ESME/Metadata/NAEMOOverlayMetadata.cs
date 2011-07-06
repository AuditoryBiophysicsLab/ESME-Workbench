
namespace ESME.Metadata
{
    public class NAEMOOverlayMetadata : NAEMOMetadataBase
    {
        public static NAEMOOverlayMetadata Load(string metaDataFilename) { return Load<NAEMOOverlayMetadata>(metaDataFilename); }

        public void Save(string filename = null) {Save(this, filename);}
    }
}

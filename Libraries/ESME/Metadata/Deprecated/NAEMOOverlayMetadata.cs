
using System.ComponentModel;
using Cinch;

namespace ESME.Metadata
{
#if false
    public class NAEMOOverlayMetadata : NAEMOMetadataBase
    {
        public static NAEMOOverlayMetadata Load(string metaDataFilename) { return Load<NAEMOOverlayMetadata>(metaDataFilename); }

        #region public float BufferZoneSize { get; set; }

        public float BufferZoneSize
        {
            get { return _bufferZoneSize; }
            set
            {
                if (_bufferZoneSize == value) return;
                _bufferZoneSize = value;
                NotifyPropertyChanged(BufferZoneSizeChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs BufferZoneSizeChangedEventArgs = ObservableHelper.CreateArgs<NAEMOOverlayMetadata>(x => x.BufferZoneSize);
        private float _bufferZoneSize;

        #endregion

        public void Save(string filename = null) { Save(this, ReferencedTypes, filename); }
    }
#endif
}

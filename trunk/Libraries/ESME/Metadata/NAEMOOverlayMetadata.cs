
using System.ComponentModel;
using Cinch;

namespace ESME.Metadata
{
    public class NAEMOOverlayMetadata : NAEMOMetadataBase
    {
        public static NAEMOOverlayMetadata Load(string metaDataFilename) { return Load<NAEMOOverlayMetadata>(metaDataFilename); }

        #region public string SourceOverlay { get; set; }

        public string SourceOverlay
        {
            get { return _sourceOverlay; }
            set
            {
                if (_sourceOverlay == value) return;
                _sourceOverlay = value;
                NotifyPropertyChanged(SourceOverlayChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs SourceOverlayChangedEventArgs = ObservableHelper.CreateArgs<NAEMOOverlayMetadata>(x => x.SourceOverlay);
        private string _sourceOverlay;

        #endregion

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

        public void Save(string filename = null) {Save(this, filename);}
    }
}

using System.ComponentModel;
using Cinch;
using ESME.Metadata;
using HRC.Validation;

namespace ESME.Views.Locations
{
    public sealed class OverlayExpandViewModel : ValidatingViewModel
    {
        public OverlayExpandViewModel(NAEMOOverlayMetadata naemoOverlayMetadata)
        {
            NAEMOOverlayMetadata = naemoOverlayMetadata;
            ValidationRules.Add(new ValidationRule
            {
                PropertyName = "BufferSize",
                Description = "Must be greater than zero.",
                RuleDelegate = (o, r) => ((OverlayExpandViewModel)o).BufferSize > 0,
            });
        }

        #region public float BufferSize { get; set; }

        public float BufferSize
        {
            get { return _bufferSize; }
            set
            {
                if (_bufferSize == value) return;
                _bufferSize = value;
                NotifyPropertyChanged(BufferSizeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BufferSizeChangedEventArgs = ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.BufferSize);
        float _bufferSize;

        #endregion

        #region public NAEMOOverlayMetadata NAEMOOverlayMetadata { get; set; }

        private static readonly PropertyChangedEventArgs NAEMOOverlayMetadataChangedEventArgs =
            ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.NAEMOOverlayMetadata);

        private NAEMOOverlayMetadata _naemoOverlayMetadata;

        public NAEMOOverlayMetadata NAEMOOverlayMetadata
        {
            get { return _naemoOverlayMetadata; }
            set
            {
                if (_naemoOverlayMetadata == value) return;
                _naemoOverlayMetadata = value;
                NotifyPropertyChanged(NAEMOOverlayMetadataChangedEventArgs);
            }
        }

        #endregion

        #region OkCommand

        private SimpleCommand<object, object> _ok;

        public SimpleCommand<object, object> OkCommand
        {
            get
            {
                return _ok ??
                       (_ok =
                        new SimpleCommand<object, object>(delegate { return IsValid; },
                                                          delegate { OkHandler(); }));
            }
        }

        private void OkHandler()
        {
            CloseActivePopUpCommand.Execute(true);
        }

        #endregion
    }
}
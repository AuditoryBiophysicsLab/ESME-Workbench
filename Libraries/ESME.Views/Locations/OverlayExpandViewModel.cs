using System;
using System.ComponentModel;
using System.IO;
using Cinch;
using ESME.Environment.Descriptors;
using HRC.Validation;

namespace ESME.Views.Locations
{
    public sealed class OverlayExpandViewModel : ValidatingViewModel
    {
        public OverlayExpandViewModel(RangeComplex rangeComplex, RangeComplexArea sourceArea)
        {
            if ((rangeComplex == null) || (sourceArea == null)) throw new ApplicationException("Range Complex and/or Area has not been selected");
            _areaName = sourceArea.Name;
            ValidationRules.Add(new ValidationRule
            {
                PropertyName = "BufferSize",
                Description = "An overlay with the same name already exists",
                RuleDelegate = (o, r) => !rangeComplex.AreaCollection.ContainsKey(OverlayName),
            });
            ValidationRules.Add(new ValidationRule
            {
                PropertyName = "BufferSize",
                Description = "Must be greater than zero.",
                RuleDelegate = (o, r) => ((OverlayExpandViewModel)o).BufferSize > 0,
            });
        }

        readonly string _areaName;
        #region public float BufferSize { get; set; }

        public float BufferSize
        {
            get { return _bufferSize; }
            set
            {
                if (Math.Abs(_bufferSize - value) < .001) return;
                _bufferSize = value;
                NotifyPropertyChanged(BufferSizeChangedEventArgs);
                NotifyPropertyChanged(OverlayNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BufferSizeChangedEventArgs = ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.BufferSize);
        float _bufferSize;

        #endregion

        #region public string OverlayName { get; set; }

        public string OverlayName
        {
            get { return string.Format("{0}_{1}km", Path.GetFileNameWithoutExtension(_areaName), BufferSize); }
        }

        static readonly PropertyChangedEventArgs OverlayNameChangedEventArgs = ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.OverlayName);

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
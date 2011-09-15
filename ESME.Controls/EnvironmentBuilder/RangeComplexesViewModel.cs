using System.Collections.Specialized;
using System.ComponentModel;
using Cinch;
using ESME.Environment.Descriptors;
using HRC.Utility;

namespace ESME.Views.EnvironmentBuilder
{
    public class RangeComplexesViewModel : ViewModelBase
    {
        public RangeComplexesViewModel(RangeComplexes rangeComplexes)
        {
            RangeComplexCollection = ObservableList<RangeComplexTreeViewModel>.FromObservableConcurrentDictionary(rangeComplexes.RangeComplexCollection,
                                                                                                        x => new RangeComplexTreeViewModel(x.Value) {Name = x.Value.Name},
                                                                                                        (x, y) => x.Value.Name == y.Name);
        }

        #region public ObservableList<RangeComplexTreeViewModel> RangeComplexCollection { get; set; }

        public ObservableList<RangeComplexTreeViewModel> RangeComplexCollection
        {
            get { return _rangeComplexCollection; }
            set
            {
                if (_rangeComplexCollection == value) return;
                _rangeComplexCollection = value;
                NotifyPropertyChanged(RangeComplexCollectionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RangeComplexCollectionChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexesViewModel>(x => x.RangeComplexCollection);
        ObservableList<RangeComplexTreeViewModel> _rangeComplexCollection;

        #endregion

    }
}

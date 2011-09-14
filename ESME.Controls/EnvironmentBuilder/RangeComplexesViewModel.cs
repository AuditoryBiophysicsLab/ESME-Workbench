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
            RangeComplexCollection = new ObservableList<RangeComplexTreeViewModel>();
            rangeComplexes.RangeComplexCollection.CollectionChanged += (s, e) =>
            {
                if (e.Action == NotifyCollectionChangedAction.Add)
                    foreach (NewRangeComplex item in e.NewItems)
                        RangeComplexCollection.Add(new RangeComplexTreeViewModel(item) { Name = item.Name });
            };
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

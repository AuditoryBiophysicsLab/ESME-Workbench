using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using Cinch;

namespace ESME.Views.TransmissionLoss
{
    public class TransmissionLossQueueCalculatorViewModel : ViewModelBase
    {
        #region public constructor

        public TransmissionLossQueueCalculatorViewModel()
        {
             FieldCalculatorViewModels = new ObservableCollection<TransmissionLossFieldCalculatorViewModel>();
        }

        #endregion

        #region public ObservableCollection<TransmissionLossFieldCalculatorViewModel> FieldCalculatorViewModels { get; set; }

        public ObservableCollection<TransmissionLossFieldCalculatorViewModel> FieldCalculatorViewModels
        {
            get { return _fieldCalculatorViewModels; }
            set
            {
                if (_fieldCalculatorViewModels == value) return;
                if (_fieldCalculatorViewModels != null) _fieldCalculatorViewModels.CollectionChanged -= FieldCalculatorViewModelsCollectionChanged;
                _fieldCalculatorViewModels = value;
                if (_fieldCalculatorViewModels != null) _fieldCalculatorViewModels.CollectionChanged += FieldCalculatorViewModelsCollectionChanged;
                NotifyPropertyChanged(FieldCalculatorViewModelsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs FieldCalculatorViewModelsChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossQueueCalculatorViewModel>(x => x.FieldCalculatorViewModels);
        ObservableCollection<TransmissionLossFieldCalculatorViewModel> _fieldCalculatorViewModels;

        void FieldCalculatorViewModelsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    if (e.NewItems != null)
                        foreach (var newItem in e.NewItems.Cast<TransmissionLossFieldCalculatorViewModel>()) {}
                    break;
                case NotifyCollectionChangedAction.Move:
                    break;
                case NotifyCollectionChangedAction.Remove:
                    if (e.OldItems != null) foreach (var oldItem in e.OldItems.Cast<TransmissionLossFieldCalculatorViewModel>()) {}
                    break;
                case NotifyCollectionChangedAction.Replace:
                    break;
                case NotifyCollectionChangedAction.Reset:
                    break;
            }
            StartWorkIfNeeded();
            NotifyPropertyChanged(FieldCalculatorViewModelsChangedEventArgs);
        }

        void StartWorkIfNeeded()
        {
            if (FieldCalculatorViewModels.Count > 0)
                FieldCalculatorViewModels[0].Start(delegate { HandleCompletedQueueItem(); });
        }

        void HandleCompletedQueueItem()
        {
            while ((FieldCalculatorViewModels.Count > 0) && (FieldCalculatorViewModels[0].IsCompleted))
            {
                var runfileName = FieldCalculatorViewModels[0].RunfileName;
                var runfilePath = Path.GetDirectoryName(runfileName);
                var outputFile = Path.Combine(runfilePath, Path.GetFileNameWithoutExtension(runfileName) + ".tlf");
                var transmissionLossField = FieldCalculatorViewModels[0].TransmissionLossField;
                transmissionLossField.Filename = outputFile;
                transmissionLossField.Save();
                FieldCalculatorViewModels.Remove(FieldCalculatorViewModels[0]);
            }
            StartWorkIfNeeded();
        }

        #endregion
    }
}
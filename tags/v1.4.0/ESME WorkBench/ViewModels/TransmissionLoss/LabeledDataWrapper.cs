using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using Cinch;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class LabeledDataWrapper<T> : DataWrapper<T>
    {
        public LabeledDataWrapper(IParentablePropertyExposer parent, PropertyChangedEventArgs parentPropertyChangeArgs) : base(parent, parentPropertyChangeArgs) { Initialize(); }

        public LabeledDataWrapper(IParentablePropertyExposer parent, PropertyChangedEventArgs parentPropertyChangeArgs, Action valuesChangedCallBack) : base(parent, parentPropertyChangeArgs, valuesChangedCallBack) { Initialize(); }

        void Initialize()
        {
            ValidationRules = new ObservableCollection<SimpleRule>();
            IsEditable = false;
        }

        #region public string Label { get; set; }

        public string Label
        {
            get { return _label; }
            set
            {
                if (_label == value) return;
                _label = value;
                NotifyPropertyChanged(LabelChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LabelChangedEventArgs = ObservableHelper.CreateArgs<LabeledDataWrapper<T>>(x => x.Label);
        string _label;

        #endregion

        #region public ObservableCollection<SimpleRule> ValidationRules { get; set; }

        public ObservableCollection<SimpleRule> ValidationRules
        {
            get { return _validationRules; }
            set
            {
                if (_validationRules == value) return;
                if (_validationRules != null) _validationRules.CollectionChanged -= ValidationRulesCollectionChanged;
                _validationRules = value;
                if (_validationRules != null) _validationRules.CollectionChanged += ValidationRulesCollectionChanged;
                NotifyPropertyChanged(ValidationRulesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ValidationRulesChangedEventArgs = ObservableHelper.CreateArgs<LabeledDataWrapper<T>>(x => x.ValidationRules);
        ObservableCollection<SimpleRule> _validationRules;

        void ValidationRulesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    if (e.NewItems != null) foreach (var newItem in e.NewItems.Cast<SimpleRule>()) AddRule(newItem);
                    break;
                case NotifyCollectionChangedAction.Move:
                case NotifyCollectionChangedAction.Remove:
                case NotifyCollectionChangedAction.Replace:
                case NotifyCollectionChangedAction.Reset:
                    throw new NotImplementedException("Removing or changing validation rules is not currently supported");
            }
            NotifyPropertyChanged(ValidationRulesChangedEventArgs);
        }

        #endregion
    }
}
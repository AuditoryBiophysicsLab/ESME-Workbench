using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Text;
using Cinch;

namespace ESMEWorkBench.ViewModels.Main
{
    class EditableLabelListViewModel:ViewModelBase
    {
        
        #region public double LabelWidth { get; set; }

        public double LabelWidth
        {
            get { return _labelWidth; }
            set
            {
                if (_labelWidth == value) return;
                _labelWidth = value;
                NotifyPropertyChanged(LabelWidthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LabelWidthChangedEventArgs = ObservableHelper.CreateArgs<EditableLabelListViewModel>(x => x.LabelWidth);
        double _labelWidth;

        #endregion

        #region public double ValueWidth { get; set; }

        public double ValueWidth
        {
            get { return _valueWidth; }
            set
            {
                if (_valueWidth == value) return;
                _valueWidth = value;
                NotifyPropertyChanged(ValueWidthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ValueWidthChangedEventArgs = ObservableHelper.CreateArgs<EditableLabelListViewModel>(x => x.ValueWidth);
        double _valueWidth;

        #endregion

        #region public ObservableCollection<LabelValuePair> ItemsSource { get; set; }

        public ObservableCollection<LabelValuePair> ItemsSource
        {
            get { return _itemsSource; }
            set
            {
                if (_itemsSource == value) return;
                if (_itemsSource != null) _itemsSource.CollectionChanged -= ItemsSourceCollectionChanged;
                _itemsSource = value;
                if (_itemsSource != null) _itemsSource.CollectionChanged += ItemsSourceCollectionChanged;
                NotifyPropertyChanged(ItemsSourceChangedEventArgs);
            }
        }

        void ItemsSourceCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(ItemsSourceChangedEventArgs); }
        static readonly PropertyChangedEventArgs ItemsSourceChangedEventArgs = ObservableHelper.CreateArgs<EditableLabelListViewModel>(x => x.ItemsSource);
        ObservableCollection<LabelValuePair> _itemsSource;

        #endregion

    }
}

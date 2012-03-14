using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using HRC.Aspects;
using PostSharp;

namespace HRC.Utility
{
    [NotifyPropertyChanged]
    public class PercentProgress : IProgress<int>
    {
        [Initialize(0.0)]
        public double MinimumValue { get; set; }

        [Initialize(100.0)]
        public double MaximumValue { get; set; }

        [Initialize(0)]
        public int PercentComplete { get; set; }
        public void Report(int value)
        {
            if (MaximumValue < value) MaximumValue = value;
            if (MinimumValue > value) MinimumValue = value;
            var percent = (int)((value / MaximumValue) * 100);
            if (PercentComplete < percent) PercentComplete = percent;
        }
    }

    public class PercentProgressList : PercentProgress, IList<PercentProgress>
    {
        readonly List<PercentProgress> _progressList = new List<PercentProgress>();
        public IEnumerator<PercentProgress> GetEnumerator() { return _progressList.GetEnumerator(); }
        IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }
        public void Add(PercentProgress item)
        {
            Post.Cast<PercentProgress, INotifyPropertyChanged>(item).PropertyChanged += (s, e) =>
            {
                if (e.PropertyName != "PercentComplete") return;
                Report(_progressList.Sum(p => p.PercentComplete) / Count);
            };
            _progressList.Add(item);
        }
        public void Clear() { _progressList.Clear(); }
        public bool Contains(PercentProgress item) { return _progressList.Contains(item); }
        public void CopyTo(PercentProgress[] array, int arrayIndex) { _progressList.CopyTo(array, arrayIndex); }
        public bool Remove(PercentProgress item) { return _progressList.Remove(item); }
        public int Count { get { return _progressList.Count; } }
        public bool IsReadOnly { get { return false; } }
        public int IndexOf(PercentProgress item) { return _progressList.IndexOf(item); }
        public void Insert(int index, PercentProgress item) { _progressList.Insert(index, item); }
        public void RemoveAt(int index) { _progressList.RemoveAt(index); }
        public PercentProgress this[int index]
        {
            get { return _progressList[index]; }
            set { _progressList[index] = value; }
        }
    }
    
    public class PercentProgress<T> : PercentProgress
    {
        public PercentProgress() {}
        public PercentProgress(T target) { ProgressTarget = target; }
        public T ProgressTarget { get; set; }
    }

    public class PercentProgressList<T> : PercentProgressList
    {
        public PercentProgressList() {}
        public PercentProgressList(T target) { ProgressTarget = target; }
        public T ProgressTarget { get; set; }
    }
}

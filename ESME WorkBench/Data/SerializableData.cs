using System.ComponentModel;

namespace ESMEWorkBench.Data
{
    internal class SerializableData : INotifyPropertyChanged
    {
        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;

        #endregion
    }
}
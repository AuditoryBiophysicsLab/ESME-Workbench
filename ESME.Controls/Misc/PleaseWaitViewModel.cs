using System.ComponentModel;
using Cinch;

namespace ESME.Views.Misc
{
    public class PleaseWaitViewModel : ViewModelBase
    {
        #region public string Message { get; set; }

        public string Message
        {
            get { return _message; }
            set
            {
                if (_message == value) return;
                _message = value;
                NotifyPropertyChanged(MessageChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MessageChangedEventArgs = ObservableHelper.CreateArgs<PleaseWaitViewModel>(x => x.Message);
        string _message;

        #endregion

        public void Close()
        {
            CloseActivePopUpCommand.Execute(true);
        }
    }
}

using System.ComponentModel;
using Cinch;

namespace ESME.Views
{
    public class TestViewModel : ViewModelBase
    {
        public TestViewModel()
        {
            TestString = "Test View";
        }

        #region public string TestString { get; set; }

        public string TestString
        {
            get { return _testString; }
            set
            {
                if (_testString == value) return;
                _testString = value;
                NotifyPropertyChanged(TestStringChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TestStringChangedEventArgs = ObservableHelper.CreateArgs<TestViewModel>(x => x.TestString);
        string _testString;

        #endregion

    }
}

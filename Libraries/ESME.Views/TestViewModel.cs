using HRC.Aspects;
using HRC.ViewModels;

namespace ESME.Views
{
    [NotifyPropertyChanged]
    public class TestViewModel : ViewModelBase
    {
        public TestViewModel()
        {
            TestString = "Test View";
        }

        public string TestString { get; set; }
    }
}

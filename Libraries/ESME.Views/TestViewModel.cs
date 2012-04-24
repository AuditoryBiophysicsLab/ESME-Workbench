using HRC.ViewModels;

namespace ESME.Views
{
    public class TestViewModel : ViewModelBase
    {
        public TestViewModel()
        {
            TestString = "Test View";
        }

        public string TestString { get; set; }
    }
}

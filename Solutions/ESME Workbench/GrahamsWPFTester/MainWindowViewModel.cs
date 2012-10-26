using ESME.Views.PSM;
using HRC.ViewModels;
using MEFedMVVM.ViewModelLocator;

namespace GrahamsWPFTester
{
    [ExportViewModel("MainWindowViewModel")]
    class MainWindowViewModel:ViewModelBase
    {
        public MainWindowViewModel()
        {
            WindowTitle = "PSM Tester";
            TreeViewModel = new PSMTreeViewModel(@"C:\Users\Graham Voysey\Desktop\");
            TreeViewModel.SeedTestValues();
        }
        

        public string WindowTitle { get; set; }
        public PSMTreeViewModel TreeViewModel { get; set; }
    }
}

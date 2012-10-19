using ESME.PSM;
using ESME.Scenarios;
using ESME.Views.PSM;
using HRC.Utility;
using HRC.ViewModels;
using MEFedMVVM.ViewModelLocator;
using System.Data.Entity;


namespace GrahamsWPFTester
{
    [ExportViewModel("MainWindowViewModel")]
    class MainWindowViewModel:ViewModelBase
    {
        static PSMContext _context;
        public MainWindowViewModel()
        {
            Seed();
            TreeViewModel = new PSMTreeViewModel();
        }
        void Seed()
        {
            _context = PSMContext.Create(@"C:\Users\Graham Voysey\Desktop");
            _context.Platforms.Add(new Platform()
            {
                PlatformName = "Platform 1",
                Sources = new ObservableList<Source>()
                {
                    new Source()
                    {
                        SourceName = "Source 1",
                        Modes = new ObservableList<Mode>()
                        {
                            new Mode()
                            {
                                ModeName = "Mode 1",
                            }
                        }
                    }
                }
            });
            _context.Platforms.Add(new Platform()
            {
                PlatformName = "Platform 2",
                Sources = new ObservableList<Source>()
                {
                    new Source()
                    {
                        SourceName = "Source 2",
                        Modes = new ObservableList<Mode>()
                        {
                            new Mode()
                            {
                                ModeName = "Mode 2",
                            }
                        }
                    }
                }
            });

        }

        public string WindowTitle { get; set; }

        public PSMTreeViewModel TreeViewModel { get; set; }
    }
}

using HRC.ViewModels;

namespace ESME.Views.PSM
{
    /// <summary>
    /// Interaction logic for PSMTreeView.xaml
    /// </summary>
    //[PopupNameToViewLookupKeyMetadata("PSMTreeView", typeof(PSMTreeView))]
    // Graham: I took this out because it makes ESME crash on startup.  This line is only needed for views derived from a Window, 
    // in other words something that will be found by the IUIVisualizerService (_visualizer.ShowDialog("AboutView", aboutViewModel));
    public partial class PSMTreeView
    {
        public PSMTreeView()
        {
            InitializeComponent();
        }
    }
}

using HRC.Aspects;
using HRC.ViewModels;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.Controls
{
    [ExportViewModel("DataAxisControlViewModel")]
    public class DataAxisControlViewModel : ViewModelBase, IDesignTimeAware
    {
        public string Label { get; set; }
        public double MaxValue { get; set; }
        public double MinValue { get; set; }
        public string AxisLocation { get; set; }
        [Initialize("0")]
        public string TickValueFormat { get; set; }

        public void DesignTimeInitialization()
        {
            Label = "DataAxisControl.Label";
            MinValue = -10;
            MaxValue = 10;
            TickValueFormat = "0.##";
            AxisLocation = "Left";
        }
    }
}
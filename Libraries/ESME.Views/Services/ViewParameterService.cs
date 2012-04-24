using System.ComponentModel;
using System.ComponentModel.Composition;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.Services
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(IViewParameterService))]

    public class ViewParameterService : IViewParameterService  
    {
        public double TransmissionLayersWidth { get; set; }

        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }

        #endregion
    }

    public interface IViewParameterService:INotifyPropertyChanged
    {
        double TransmissionLayersWidth { get; set; }
    }
}

using System.ComponentModel;
using System.ComponentModel.Composition;
using HRC.ViewModels;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.Services
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(IViewParameterService))]
    public class ViewParameterService : ViewModelBase, IViewParameterService  
    {
        public double TransmissionLayersWidth { get; set; }
    }

    public interface IViewParameterService:INotifyPropertyChanged
    {
        double TransmissionLayersWidth { get; set; }
    }
}

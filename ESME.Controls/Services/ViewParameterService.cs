using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Linq;
using System.Text;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.Services
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(IViewParameterService))]

    public class ViewParameterService:IViewParameterService  
    {
        #region public double TransmissionLayersWidth { get; set; }

        public double TransmissionLayersWidth
        {
            get { return _transmissionLayersWidth; }
            set
            {
                if (_transmissionLayersWidth == value) return;
                _transmissionLayersWidth = value;
                NotifyPropertyChanged(TransmissionLayersWidthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLayersWidthChangedEventArgs = ObservableHelper.CreateArgs<ViewParameterService>(x => x.TransmissionLayersWidth);
        double _transmissionLayersWidth;

        #endregion

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

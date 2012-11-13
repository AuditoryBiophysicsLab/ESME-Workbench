using ESME.Scenarios;
using HRC.Aspects;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Scenarios
{
    /// <summary>
    /// To create and show the view as a dialog:
    /// var vm = new CreatePlatformViewModel {...};
    /// var result = _visualizerService.ShowDialog("CreatePlatformView", vm);
    /// if ((!result.HasValue) || (!result.Value)) return;
    /// 
    /// To create and show the view as a window:
    /// var vm = new CreatePlatformViewModel {...};
    /// var window = _visualizerService.ShowWindow("CreatePlatformView", vm);
    /// </summary>
    
    public class PropertiesViewModel : ViewModelBase
    {
        public string WindowTitle { get; set; }
        public object PropertyObject { get; set; }
        public bool IsPSMView { get; set; }
        public bool IsNew { get; set; }
        object _propertyObject;
        public PropertiesViewModel(object propertyObject)
        {
            PropertyObject = propertyObject;
            _propertyObject = propertyObject;
        }

        public PropertiesViewModel() {
            
        }

        #region OkCommand
        public SimpleCommand<object, EventToCommandArgs> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, EventToCommandArgs>(OkHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _ok;

        void OkHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter;
            if (!IsPSMView)
            {
                CloseDialog(true);
            }
            else
            {
                if (IsNew)
                {
                    if (PropertyObject is Platform)
                    {
                        MediatorMessage.Send(MediatorMessage.PSMPlatformAdded, PropertyObject);
                    }
                    if (PropertyObject is Source)
                    {
                        MediatorMessage.Send(MediatorMessage.PSMSourceAdded, PropertyObject);
                    }
                }
                else
                {
                    if (!_propertyObject.Equals(PropertyObject))
                    {
                        if (PropertyObject is Platform)
                        {
                             MediatorMessage.Send(MediatorMessage.PSMPlatformChanged, PropertyObject);
                        }
                        if (PropertyObject is Source)
                        {
                            MediatorMessage.Send(MediatorMessage.PSMSourceChanged, PropertyObject);
                        }
                    }
                }
            }
        }
        #endregion
    }
}

using ESME.Scenarios;
using HRC.Aspects;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Scenarios
{
    public class ModeDiagramViewModel : ViewModelBase
    {
        public double RangeMin { get; set; }
        public double RangeMax { get; set; }
        public double DepthMax { get; set; }
        public double DepthMin { get; set; }

        Mode _mode;
        [Affects("RangeMin", "RangeMax", "DepthMin", "DepthMax")]
        public Mode Mode
        {
            get { return _mode; }
            set
            {
                _mode = value;
                RangeMin = 0;
                RangeMax = _mode.MaxPropagationRadius;
            }
        }

        public ModeDiagramViewModel(Mode mode) { Mode = mode; }

        #region commands
        #region SizeChangedCommand
        public SimpleCommand<object, EventToCommandArgs> SizeChangedCommand { get { return _sizeChanged ?? (_sizeChanged = new SimpleCommand<object, EventToCommandArgs>(SizeChangedHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _sizeChanged;

        static void SizeChangedHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter;
        }
        #endregion
        #endregion
    }
}

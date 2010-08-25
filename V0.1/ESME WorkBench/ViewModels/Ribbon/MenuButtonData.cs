using System.ComponentModel;

namespace ESMERibbonDemo.ViewModels.Ribbon
{
    public class MenuButtonData : ControlData
    {
        public bool IsVerticallyResizable
        {
            get
            {
                return _isVerticallyResizable;
            }

            set
            {
                if (_isVerticallyResizable != value)
                {
                    _isVerticallyResizable = value;
                    OnPropertyChanged(new PropertyChangedEventArgs("IsVerticallyResizable"));
                }
            }
        }

        public bool IsHorizontallyResizable
        {
            get
            {
                return _isHorizontallyResizable;
            }

            set
            {
                if (_isHorizontallyResizable != value)
                {
                    _isHorizontallyResizable = value;
                    OnPropertyChanged(new PropertyChangedEventArgs("IsHorizontallyResizable"));
                }
            }
        }

        private bool _isVerticallyResizable, _isHorizontallyResizable;
    }
}

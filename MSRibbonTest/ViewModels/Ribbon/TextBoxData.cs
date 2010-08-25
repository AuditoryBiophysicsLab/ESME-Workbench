using System.ComponentModel;

namespace ESMERibbonDemo.ViewModels.Ribbon
{
    public class TextBoxData : ControlData
    {
        public string Text
        {
            get
            {
                return _text;
            }

            set
            {
                if (_text != value)
                {
                    _text = value;
                    OnPropertyChanged(new PropertyChangedEventArgs("Text"));
                }
            }
        }
        private string _text;
    }
}

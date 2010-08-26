using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Windows.Controls;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMERibbonDemo.ViewModels.Ribbon
{
    [ExportViewModel("TextBoxDataViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class TextBoxDataViewModel : ControlDataViewModel
    {
        public string Text
        {
            get { return _text; }
            set
            {
                if (_text == value) return;
                _text = value;
                NotifyPropertyChanged(TextChangedEventArgs);
            }
        }
        private static readonly PropertyChangedEventArgs TextChangedEventArgs = ObservableHelper.CreateArgs<TextBoxDataViewModel>(x => x.Text);
        private string _text;
    }
}

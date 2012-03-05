using System.ComponentModel;
using System.ComponentModel.Composition;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkbench.ViewModels.Ribbon
{
    [ExportViewModel("TextBoxDataViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class TextBoxDataViewModel : ControlDataViewModel
    {
        static readonly PropertyChangedEventArgs TextChangedEventArgs = ObservableHelper.CreateArgs<TextBoxDataViewModel>(x => x.Text);
        string _text;

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
    }
}
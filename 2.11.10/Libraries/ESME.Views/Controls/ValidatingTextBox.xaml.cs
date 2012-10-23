using System.Windows;
using System.Windows.Data;

namespace ESME.Views.Controls
{
    /// <summary>
    /// Interaction logic for ValidatingTextBox.xaml
    /// </summary>
    public partial class ValidatingTextBox
    {
        public ValidatingTextBox()
        {
            InitializeComponent();
        }

        #region dependency property string BoundField

        public static DependencyProperty BoundFieldProperty = DependencyProperty.Register("BoundField", typeof(string), typeof(ValidatingTextBox),
                                                                                        new FrameworkPropertyMetadata(string.Empty, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, BoundFieldPropertyChanged));

        public string BoundField
        {
            get { return (string)GetValue(BoundFieldProperty); }
            set { SetCurrentValue(BoundFieldProperty, value); }
        }

        public static void BoundFieldPropertyChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var destBinding = new Binding(((ValidatingTextBox)d).BoundField)
            {
                ValidatesOnDataErrors = true,
                ValidatesOnExceptions = true,
                NotifyOnValidationError = true,
                UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged
            };
            BindingOperations.SetBinding(d, TextProperty, destBinding);
        }

        #endregion

    }
}

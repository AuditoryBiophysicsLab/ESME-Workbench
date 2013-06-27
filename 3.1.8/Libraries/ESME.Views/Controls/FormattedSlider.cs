using System.Reflection;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;

namespace ESME.Views.Controls
{
    /// <summary>
    /// A Slider which provides a way to modify the 
    /// auto tooltip text by using a format string.
    /// </summary>
    public class FormattedSlider : Slider
    {
        private ToolTip _autoToolTip;

        /// <summary>
        /// Gets/sets a format string used to modify the auto tooltip's content.
        /// Note: This format string must contain exactly one placeholder value,
        /// which is used to hold the tooltip's original content.
        /// </summary>
        #region dependency property string AutoToolTipFormat

        public static DependencyProperty AutoToolTipFormatProperty = DependencyProperty.Register("AutoToolTipFormat",
                                                                                 typeof(string),
                                                                                 typeof(FormattedSlider),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, AutoToolTipFormatPropertyChanged));

        public string AutoToolTipFormat { get { return (string)GetValue(AutoToolTipFormatProperty); } set { SetValue(AutoToolTipFormatProperty, value); } }

        static void AutoToolTipFormatPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((FormattedSlider)obj).AutoToolTipFormatPropertyChanged(); }
        void AutoToolTipFormatPropertyChanged() { }
        #endregion

        #region dependency property double AutoToolTipValue

        public static DependencyProperty AutoToolTipValueProperty = DependencyProperty.Register("AutoToolTipValue",
                                                                                 typeof(double),
                                                                                 typeof(FormattedSlider),
                                                                                 new FrameworkPropertyMetadata(double.NaN));

        public double AutoToolTipValue { get { return (double)GetValue(AutoToolTipValueProperty); } set { SetValue(AutoToolTipValueProperty, value); } }

        #endregion

        protected override void OnThumbDragStarted(DragStartedEventArgs e)
        {
            base.OnThumbDragStarted(e);
            FormatAutoToolTipContent();
        }

        protected override void OnThumbDragDelta(DragDeltaEventArgs e)
        {
            base.OnThumbDragDelta(e);
            FormatAutoToolTipContent();
        }

        private void FormatAutoToolTipContent()
        {
            if (!string.IsNullOrEmpty(AutoToolTipFormat))
                AutoToolTip.Content = string.Format(AutoToolTipFormat, double.IsNaN(AutoToolTipValue) ? AutoToolTip.Content : AutoToolTipValue);
        }

        private ToolTip AutoToolTip
        {
            get
            {
                if (_autoToolTip == null)
                {
                    var field = typeof(Slider).GetField("_autoToolTip", BindingFlags.NonPublic | BindingFlags.Instance);

                    if (field != null) _autoToolTip = field.GetValue(this) as ToolTip;
                }

                return _autoToolTip;
            }
        }
    }
}

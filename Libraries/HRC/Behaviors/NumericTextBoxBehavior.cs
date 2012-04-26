using System.Text.RegularExpressions;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Interactivity;

namespace HRC.Behaviors
{
    /// <summary>
    /// A simple Numeric text based Behavior which can be applied to TextBox
    /// </summary>
    /// <remarks>
    /// Recommended usage:
    /// <code>
    /// IN YOUR VIEW HAVE SOMETHING LIKE THIS
    /// 
    /// 
    ///         xmlns:i="clr-namespace:System.Windows.Interactivity;assembly=System.Windows.Interactivity"
    ///         xmlns:behaviors="clr-namespace:HRC.Behaviors;assembly=HRC"
    /// 
    ///         <TextBox Width="150" Text="{Binding Age}" Margin="179.5,10">
    ///             <i:Interaction.Behaviors>
    ///                 <hrc:NumericTextBoxBehaviour/>
    ///             </i:Interaction.Behaviors>
    ///         </TextBox>
    /// </code>
    /// </remarks>
    public class NumericTextBoxBehavior : Behavior<TextBox>
    {
        #region Overrides
        protected override void OnAttached()
        {
            base.OnAttached();
            AssociatedObject.PreviewTextInput += PreviewTextInput;
            DataObject.AddPastingHandler(AssociatedObject, OnClipboardPaste);
        }

        protected override void OnDetaching()
        {
            AssociatedObject.PreviewTextInput -= PreviewTextInput;
            DataObject.RemovePastingHandler(AssociatedObject, OnClipboardPaste);
        }
        #endregion

        #region Private Methods
        private static void PreviewTextInput(object sender, TextCompositionEventArgs e)
        {
            var tb = sender as TextBox;

            if (tb != null && !Validate(tb, e.Text))
                e.Handled = true;
        }

        #region dependency property double LowerLimit

        public static DependencyProperty LowerLimitProperty = DependencyProperty.Register("LowerLimit", typeof (double), typeof (NumericTextBoxBehavior),
                                                                                 new FrameworkPropertyMetadata(double.MinValue));

        public double LowerLimit
        {
            get { return (double)GetValue(LowerLimitProperty); }
            set { SetCurrentValue(LowerLimitProperty, value); }
        }

        #endregion

        #region dependency property double UpperLimit

        public static DependencyProperty UpperLimitProperty = DependencyProperty.Register("UpperLimit", typeof (double), typeof (NumericTextBoxBehavior),
                                                                                 new FrameworkPropertyMetadata(double.MaxValue));

        public double UpperLimit
        {
            get { return (double)GetValue(UpperLimitProperty); }
            set { SetCurrentValue(UpperLimitProperty, value); }
        }

        #endregion

        #region dependency property bool IsInteger

        public static DependencyProperty IsIntegerProperty = DependencyProperty.Register("IsInteger", typeof (bool), typeof (NumericTextBoxBehavior),
                                                                                 new FrameworkPropertyMetadata(false));

        public bool IsInteger
        {
            get { return (bool)GetValue(IsIntegerProperty); }
            set { SetCurrentValue(IsIntegerProperty, value); }
        }

        #endregion



#if !SILVERLIGHT
        /// <summary>
        /// This method handles paste and drag/drop events onto the TextBox.  It restricts the character
        /// set to numerics and ensures we have consistent behavior. 
        /// This is only available in WPF
        /// </summary>
        /// <param name="sender">TextBox sender</param>
        /// <param name="e">EventArgs</param>
        private static void OnClipboardPaste(object sender, DataObjectPastingEventArgs e)
        {
            var tb = sender as TextBox;
            var text = e.SourceDataObject.GetData(e.FormatToApply) as string;

            if (tb != null && !string.IsNullOrEmpty(text) && !Validate(tb, text))
                e.CancelCommand();
        }
#endif

        private static bool Validate(TextBox tb, string newContent)
        {
            string testString;
            // replace selection with new text.
            if (!string.IsNullOrEmpty(tb.SelectedText))
            {
                var pre = tb.Text.Substring(0, tb.SelectionStart);
                var after = tb.Text.Substring(tb.SelectionStart + tb.SelectionLength,
                    tb.Text.Length - (tb.SelectionStart + tb.SelectionLength));
                testString = pre + newContent + after;
            }
            else
            {
                var pre = tb.Text.Substring(0, tb.CaretIndex);
                var after = tb.Text.Substring(tb.CaretIndex, tb.Text.Length - tb.CaretIndex);
                testString = pre + newContent + after;
            }

            var regExpr = new Regex(@"^([-+]?)(\d*)([,.]?)(\d*)$");
            if (regExpr.IsMatch(testString))
                return true;

            return false;
        }
        #endregion
    }
}

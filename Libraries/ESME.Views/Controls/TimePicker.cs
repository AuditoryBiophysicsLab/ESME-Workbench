using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using System.Windows.Input;

namespace ESME.Views.Controls
{
    /// <summary>
    ///   Time Picker as a control that lets the user select a specific time
    /// </summary>
    [TemplatePart(Name = "PART_Hours", Type = typeof(TextBox)),
     TemplatePart(Name = "PART_Minutes", Type = typeof(TextBox)),
     TemplatePart(Name = "PART_Seconds", Type = typeof(TextBox)),
     TemplatePart(Name = "PART_Milliseconds", Type = typeof(TextBox)),
     TemplatePart(Name = "PART_IncreaseTime", Type = typeof(ButtonBase)),
     TemplatePart(Name = "PART_DecrementTime", Type = typeof(ButtonBase))]
    public class TimePicker : Control
    {
        int _hourMaxValue = 23;
        int _minuteMaxValue = 59;
        int _secondMaxValue = 59;
        int _millisecondMaxValue = 999;
        int _hourMinValue;
        int _minuteMinValue;
        int _secondMinValue;
        int _millisecondMinValue;

        //data memebers to store the textboxes for hours, minutes and seconds
        TextBox _hours, _minutes, _seconds, _milliseconds;

        //the textbox that is selected
        TextBox _currentlySelectedTextBox;

        #region Properties
        /// <summary>
        ///   Gets or sets the minimum time that can be selected
        /// </summary>
        public TimeSpan MinTime { get { return (TimeSpan)GetValue(MinTimeProperty); } set { SetValue(MinTimeProperty, value); } }

        /// <summary>
        ///   Gets or sets the minimum time selected
        /// </summary>
        public static readonly DependencyProperty MinTimeProperty =
            DependencyProperty.Register("MinTime",
                                        typeof(TimeSpan),
                                        typeof(TimePicker),
                                        new UIPropertyMetadata(TimeSpan.MinValue,
                                                               delegate(DependencyObject sender, DependencyPropertyChangedEventArgs e)
                                                               {
                                                                   var picker = (TimePicker)sender;
                                                                   picker._hourMinValue = picker.MinTime.Hours;
                                                                   picker._minuteMinValue = picker.MinTime.Minutes;
                                                                   picker._secondMinValue = picker.MinTime.Seconds;
                                                                   picker._millisecondMinValue = picker.MinTime.Milliseconds;
                                                                   picker.CoerceValue(SelectedTimeProperty); //make sure to update the time if appropiate
                                                               }));

        /// <summary>
        ///   Gets or sets the maximum time that can be selected
        /// </summary>
        public TimeSpan MaxTime { get { return (TimeSpan)GetValue(MaxTimeProperty); } set { SetValue(MaxTimeProperty, value); } }

        /// <summary>
        ///   Gets or sets the maximum time that can be selected
        /// </summary>
        public static readonly DependencyProperty MaxTimeProperty =
            DependencyProperty.Register("MaxTime",
                                        typeof(TimeSpan),
                                        typeof(TimePicker),
                                        new UIPropertyMetadata(TimeSpan.MaxValue,
                                                               delegate(DependencyObject sender, DependencyPropertyChangedEventArgs e)
                                                               {
                                                                   var picker = (TimePicker)sender;
                                                                   picker._hourMaxValue = picker.MaxTime.Hours;
                                                                   picker._minuteMaxValue = picker.MaxTime.Minutes;
                                                                   picker._secondMaxValue = picker.MaxTime.Seconds;
                                                                   picker._millisecondMaxValue = picker.MaxTime.Milliseconds;
                                                                   picker.CoerceValue(SelectedTimeProperty); //make sure to update the time if appropiate
                                                               }));

        /// <summary>
        ///   Gets or sets the selected timestamp
        /// </summary>
        public TimeSpan SelectedTime { get { return (TimeSpan)GetValue(SelectedTimeProperty); } set { SetValue(SelectedTimeProperty, value); } }

        /// <summary>
        ///   Backing store for the selected timestamp
        /// </summary>
        public static readonly DependencyProperty SelectedTimeProperty =
            DependencyProperty.Register("SelectedTime",
                                        typeof(TimeSpan),
                                        typeof(TimePicker),
                                        new UIPropertyMetadata(new TimeSpan(0, 0, 0),
                                                               SelectedTimePropertyChanged,
                                                               ForceValidSelectedTime));

        //make sure tha the proper time is set
        static object ForceValidSelectedTime(DependencyObject sender, object value)
        {
            var picker = (TimePicker)sender;
            var time = (TimeSpan)value;
            if (time < picker.MinTime) return picker.MinTime;
            return time > picker.MaxTime ? picker.MaxTime : time;
        }

        static void SelectedTimePropertyChanged(DependencyObject sender,
                                                DependencyPropertyChangedEventArgs e)
        {
            var timePicker = (TimePicker)sender;
            var newTime = (TimeSpan)e.NewValue;
            var oldTime = (TimeSpan)e.OldValue;

            if (!timePicker._isUpdatingTime)
            {
                timePicker.BeginUpdateSelectedTime(); //signal that the selected time is being updated
                timePicker.SelectedHour = newTime.Hours;
                timePicker.SelectedMinute = newTime.Minutes;
                timePicker.SelectedSecond = newTime.Seconds;
                timePicker.SelectedMillisecond = newTime.Milliseconds;
                timePicker.EndUpdateSelectedTime(); //signal that the selected time has been updated
                timePicker.OnTimeSelectedChanged(timePicker.SelectedTime, oldTime);
            }
        }

        /// <summary>
        ///   Gets or sets the selected Hour
        /// </summary>
        public int SelectedHour { get { return (int)GetValue(SelectedHourProperty); } set { SetValue(SelectedHourProperty, value); } }

        /// <summary>
        ///   Backing store for the selected hour
        /// </summary>
        public static readonly DependencyProperty SelectedHourProperty =
            DependencyProperty.Register("SelectedHour",
                                        typeof(int),
                                        typeof(TimePicker),
                                        new UIPropertyMetadata(0,
                                                               delegate(DependencyObject sender, DependencyPropertyChangedEventArgs e)
                                                               {
                                                                   var timePicker = (TimePicker)sender;

                                                                   //validate the hour set
                                                                   timePicker.SelectedHour = ValidateNumber(timePicker.SelectedHour,
                                                                                                            timePicker._hourMinValue,
                                                                                                            timePicker._hourMaxValue);
                                                                   //set the new timespan
                                                                   SetNewTime(timePicker);
                                                               }));

        /// <summary>
        ///   Gets or sets the selected minutes
        /// </summary>
        public int SelectedMinute { get { return (int)GetValue(SelectedMinuteProperty); } set { SetValue(SelectedMinuteProperty, value); } }

        /// <summary>
        ///   Backing store for the selected minsutes
        /// </summary>
        public static readonly DependencyProperty SelectedMinuteProperty =
            DependencyProperty.Register("SelectedMinute",
                                        typeof(int),
                                        typeof(TimePicker),
                                        new UIPropertyMetadata(0,
                                                               delegate(DependencyObject sender, DependencyPropertyChangedEventArgs e)
                                                               {
                                                                   var timePicker = (TimePicker)sender;

                                                                   //validate the minute set
                                                                   timePicker.SelectedMinute = ValidateNumber(timePicker.SelectedMinute,
                                                                                                              timePicker._minuteMinValue,
                                                                                                              timePicker._minuteMaxValue);
                                                                   //set the new timespan
                                                                   SetNewTime(timePicker);
                                                               }));

        /// <summary>
        ///   Gets or sets the selected second
        /// </summary>
        public int SelectedSecond { get { return (int)GetValue(SelectedSecondProperty); } set { SetValue(SelectedSecondProperty, value); } }

        /// <summary>
        ///   Backing store for the selected second
        /// </summary>
        public static readonly DependencyProperty SelectedSecondProperty =
            DependencyProperty.Register("SelectedSecond",
                                        typeof(int),
                                        typeof(TimePicker),
                                        new UIPropertyMetadata(0,
                                                               delegate(DependencyObject sender, DependencyPropertyChangedEventArgs e)
                                                               {
                                                                   var timePicker = (TimePicker)sender;

                                                                   //validate the minute set
                                                                   timePicker.SelectedSecond = ValidateNumber(timePicker.SelectedSecond,
                                                                                                              timePicker._secondMinValue,
                                                                                                              timePicker._secondMaxValue);
                                                                   //set the new timespan
                                                                   SetNewTime(timePicker);
                                                               }));

        /// <summary>
        ///   Gets or sets the selected second
        /// </summary>
        public int SelectedMillisecond { get { return (int)GetValue(SelectedMillisecondProperty); } set { SetValue(SelectedMillisecondProperty, value); } }

        /// <summary>
        ///   Backing store for the selected second
        /// </summary>
        public static readonly DependencyProperty SelectedMillisecondProperty =
            DependencyProperty.Register("SelectedMillisecond",
                                        typeof(int),
                                        typeof(TimePicker),
                                        new UIPropertyMetadata(0,
                                                               delegate(DependencyObject sender, DependencyPropertyChangedEventArgs e)
                                                               {
                                                                   var timePicker = (TimePicker)sender;

                                                                   //validate the minute set
                                                                   timePicker.SelectedMillisecond = ValidateNumber(timePicker.SelectedMillisecond,
                                                                                                                   timePicker._millisecondMinValue,
                                                                                                                   timePicker._millisecondMaxValue);

                                                                   //set the new timespan
                                                                   SetNewTime(timePicker);
                                                               }));
        #endregion

        #region Events
        /// <summary>
        /// </summary>
        public static readonly RoutedEvent SelectedTimeChangedEvent = EventManager.RegisterRoutedEvent("SelectedTimeChanged",
                                                                                                       RoutingStrategy.Bubble,
                                                                                                       typeof(TimeSelectedChangedEventHandler),
                                                                                                       typeof(TimePicker));

        public event TimeSelectedChangedEventHandler SelectedTimeChanged { add { AddHandler(SelectedTimeChangedEvent, value); } remove { RemoveHandler(SelectedTimeChangedEvent, value); } }
        #endregion

        /// <summary>
        ///   Default constructor
        /// </summary>
        public TimePicker() { SelectedTime = DateTime.Now.TimeOfDay; }

        /// <summary>
        ///   Static constructor
        /// </summary>
        static TimePicker()
        {
            DefaultStyleKeyProperty.OverrideMetadata(
                typeof(TimePicker),
                new FrameworkPropertyMetadata(typeof(TimePicker)
                    ));
        }

        /// <summary>
        ///   override to hook to the Control template elements
        /// </summary>
        public override void OnApplyTemplate()
        {
            //get the hours textbox and hook the events to it
            _hours = GetTemplateChild("PART_Hours") as TextBox;
            if (_hours == null) throw new ApplicationException("PART_Hours textbox not found!");
            _hours.PreviewTextInput += HoursTextChanged;
            _hours.KeyUp += HoursKeyUp;
            _hours.GotFocus += TextGotFocus;
            _hours.GotMouseCapture += TextGotFocus;

            //get the minutes textbox and hook the events to it
            _minutes = GetTemplateChild("PART_Minutes") as TextBox;
            if (_minutes == null) throw new ApplicationException("PART_Minutes textbox not found!");
            _minutes.PreviewTextInput += MinutesTextChanged;
            _minutes.KeyUp += MinutesKeyUp;
            _minutes.GotFocus += TextGotFocus;
            _minutes.GotMouseCapture += TextGotFocus;

            //get the seconds textbox and hook the events to it
            _seconds = GetTemplateChild("PART_Seconds") as TextBox;
            if (_seconds == null) throw new ApplicationException("PART_Seconds textbox not found!");
            _seconds.PreviewTextInput += SecondsTextChanged;
            _seconds.KeyUp += SecondsKeyUp;
            _seconds.GotFocus += TextGotFocus;
            _seconds.GotMouseCapture += TextGotFocus;

            //get the seconds textbox and hook the events to it
            _milliseconds = GetTemplateChild("PART_Milliseconds") as TextBox;
            if (_milliseconds == null) throw new ApplicationException("PART_Milliseconds textbox not found!");
            _milliseconds.PreviewTextInput += MillisecondsTextChanged;
            _milliseconds.KeyUp += MillisecondsKeyUp;
            _milliseconds.GotFocus += TextGotFocus;
            _milliseconds.GotMouseCapture += TextGotFocus;

            //Get the increase button and hook to the click event
            var increaseButton = GetTemplateChild("PART_IncreaseTime") as ButtonBase;
            if (increaseButton == null) throw new ApplicationException("PART_IncreaseTime button not found!");
            increaseButton.Click += IncreaseTime;
            //Get the decrease button and hook to the click event
            var decrementButton = GetTemplateChild("PART_DecrementTime") as ButtonBase;
            if (decrementButton == null) throw new ApplicationException("PART_DecrementTime button not found!");
            decrementButton.Click += DecrementTime;
        }

        //event handler for the textboxes (hours, minutes, seconds)
        void TextGotFocus(object sender, RoutedEventArgs e)
        {
            var selectedBox = (TextBox)sender;
            //set the currently selected textbox. 
            //This field is used to check which entity(hour/minute/second) to increment/decrement when user clicks the buttuns
            _currentlySelectedTextBox = selectedBox;

            //highlight all code so that it is easier to the user to enter new info in the text box
            selectedBox.SelectAll();
        }

        #region preview input handler
        //handle the preview event so that we validate the text before it is set in the textbox's text

        //event handler for the Hour TextBox
        void HoursTextChanged(object sender, TextCompositionEventArgs e)
        {
            //delete the text that is highlight(selected)
            TrimSelectedText(_hours);

            //Adjust the text according to the caret index
            var newText = AdjustText(_hours, e.Text);

            //validates that the hour is correct if not set a valid value (0 or 24)
            ValidateAndSetHour(newText);

            //moves the caret index or focus the neighbour
            AdjustCaretIndexOrMoveToNeighbour(_hours, _minutes);

            //handle the event so that it does not set the text, since we do it manually
            e.Handled = true;
        }

        //event handler for the Minute TextBox
        void MinutesTextChanged(object sender, TextCompositionEventArgs e)
        {
            //delete the text that is highlight(selected)
            TrimSelectedText(_minutes);

            //Adjust the text according to the caret index
            var newText = AdjustText(_minutes, e.Text);

            //validates that the minute is correct if not set a valid value (0 or 59)
            ValidateAndSetMinute(newText);

            //moves the caret index or focus the neighbour
            AdjustCaretIndexOrMoveToNeighbour(_minutes, _seconds);

            //handle the event so that it does not set the text, since we do it manually
            e.Handled = true;
        }

        //event handler for the Second TextBox
        void SecondsTextChanged(object sender, TextCompositionEventArgs e)
        {
            //delete the text that is highlight(selected)
            TrimSelectedText(_seconds);

            //Adjust the text according to the caret index
            var newText = AdjustText(_seconds, e.Text);

            //validates that the second is correct if not set a valid value (0 or 59)
            ValidateAndSetSeconds(newText);

            //moves the caret index or focus the neighbour
            AdjustCaretIndexOrMoveToNeighbour(_seconds, _milliseconds);

            //handle the event so that it does not set the text, since we do it manually
            e.Handled = true;
        }

        //event handler for the Second TextBox
        void MillisecondsTextChanged(object sender, TextCompositionEventArgs e)
        {
            //delete the text that is highlight(selected)
            TrimSelectedText(_milliseconds);
            
            //Adjust the text according to the caret index
            var newText = AdjustText(_milliseconds, e.Text);

            //validates that the second is correct if not set a valid value (0 or 59)
            ValidateAndSetMilliseconds(newText);

            //moves the caret index or focus the neighbour
            AdjustCaretIndexOrMoveToNeighbour(_milliseconds, null);

            //handle the event so that it does not set the text, since we do it manually
            e.Handled = true;
        }
        #endregion

        #region key up handlers
        //increments/decrement the selected time accordingly to the selected control
        bool IncrementDecrementTime(Key selectedKey)
        {
            switch (selectedKey)
            {
                case Key.Up:
                    IncrementDecrementTime(true);
                    break;
                case Key.Down:
                    IncrementDecrementTime(false);
                    break;
                default:
                    return false;
            }
            return true;
        }

        void HoursKeyUp(object sender, KeyEventArgs e)
        {
            //focus the next control
            TryFocusNeighbourControl(_hours, null, _minutes, e.Key);

            if (!IncrementDecrementTime(e.Key)) ValidateAndSetHour(_hours.Text);
        }

        void MinutesKeyUp(object sender, KeyEventArgs e)
        {
            //focus the next control
            TryFocusNeighbourControl(_minutes, _hours, _seconds, e.Key);

            if (!IncrementDecrementTime(e.Key)) ValidateAndSetMinute(_minutes.Text);
        }

        void SecondsKeyUp(object sender, KeyEventArgs e)
        {
            //focus the next control
            TryFocusNeighbourControl(_seconds, _minutes, _milliseconds, e.Key);

            if (!IncrementDecrementTime(e.Key)) ValidateAndSetSeconds(_seconds.Text);
        }

        void MillisecondsKeyUp(object sender, KeyEventArgs e)
        {
            //focus the next control
            TryFocusNeighbourControl(_milliseconds, _seconds, null, e.Key);

            if (!IncrementDecrementTime(e.Key)) ValidateAndSetMilliseconds(_milliseconds.Text);
        }
        #endregion

        #region increase decrease button handlers
        //event handler for the decrease button click
        void DecrementTime(object sender, RoutedEventArgs e) { IncrementDecrementTime(false); }

        void IncreaseTime(object sender, RoutedEventArgs e) { IncrementDecrementTime(true); }
        #endregion

        #region Helper methods
        //increment or decrement the time (hour/minute/second) currently in selected (determined by the currentlySelectedTextBox that is set in the GotFocus event of the textboxes)
        void IncrementDecrementTime(bool increment)
        {
            //check if hour is selected if yes set it
            if (_hours == _currentlySelectedTextBox) SelectedHour = IncrementDecrementNumber(_hours.Text, _hourMinValue, _hourMaxValue, increment);
            //check if minute is selected if yes set it
            else if (_minutes == _currentlySelectedTextBox) SelectedMinute = IncrementDecrementNumber(_minutes.Text, _minuteMinValue, _minuteMaxValue, increment);
            //check if second is selected if yes set it
            else if (_seconds == _currentlySelectedTextBox) SelectedSecond = IncrementDecrementNumber(_seconds.Text, _secondMinValue, _secondMaxValue, increment);
            //if none of the above are selected assume that milliseconds is selected
            else if (_milliseconds == _currentlySelectedTextBox) SelectedMillisecond = IncrementDecrementNumber(_milliseconds.Text, _millisecondMinValue, _millisecondMaxValue, increment);
        }

        #region Validate and set properties
        //validates the hour passed as text and sets it to the SelectedHour property
        void ValidateAndSetHour(string text)
        {
            SelectedHour = ValidateNumber(text, _hourMinValue, _hourMaxValue);
        }

        //validates the minute passed as text and sets it to the SelectedMinute property
        void ValidateAndSetMinute(string text)
        {
            SelectedMinute = ValidateNumber(text, _minuteMinValue, _minuteMaxValue);
        }

        //validates the second passed as text and sets it to the SelectedSecond property
        void ValidateAndSetSeconds(string text)
        {
            SelectedSecond = ValidateNumber(text, _secondMinValue, _secondMaxValue);
        }

        //validates the second passed as text and sets it to the SelectedMillisecond property
        void ValidateAndSetMilliseconds(string text)
        {
            SelectedMillisecond = ValidateNumber(text, _millisecondMinValue, _millisecondMaxValue);
        }
        #endregion

        //focuses the left/right control accordingly to the key passed. Pass null if there is not a neighbour control
        static void TryFocusNeighbourControl(TextBox currentControl,
                                             IInputElement leftControl,
                                             IInputElement rightControl,
                                             Key keyPressed)
        {
            if (keyPressed == Key.Left &&
                leftControl != null &&
                currentControl.CaretIndex == 0)
            {
                leftControl.Focus();
            }

            else if (keyPressed == Key.Right &&
                     rightControl != null &&
                     //if the caret index is the same as the length of the text and the user clicks right key it means that he wants to go to the next textbox
                     currentControl.CaretIndex == currentControl.Text.Length)
            {
                rightControl.Focus();
            }
        }

        //remove the left hand side number if the caret index is 0 if the caret index is 1 it removes the right hand side text
        static string AdjustText(TextBox textBox, string newText)
        {
            //replace the new text with the old text if there are already 2 char in the textbox
            if (textBox.Text.Length == 2)
            {
                if (textBox.CaretIndex == 0) return newText + textBox.Text[1];
                return textBox.Text[0] + newText;
            }
            return textBox.CaretIndex == 0
                       ? newText + textBox.Text //if the caret is in front the text append the new text infront
                       : textBox.Text + newText; //else put it in behind the existing text
        }

        //moves the caret for the textbox and if the caret is at the end it will focus the neighbour
        static void AdjustCaretIndexOrMoveToNeighbour(TextBox current, IInputElement neighbour)
        {
            //if the current is near the end move to neighbour
            if (current.CaretIndex == 1 && neighbour != null) neighbour.Focus();

                //if the caret is in the first index move the caret one index
            else if (current.CaretIndex == 0) current.CaretIndex++;
        }

        //Removes the selected text
        static void TrimSelectedText(TextBox textBox) { if (textBox.SelectionLength > 0) textBox.Text = textBox.Text.Remove(textBox.SelectionStart, textBox.SelectionLength); }

        //sets the selectedTime with the selectedhour, selectedminute and selectedsecond
        static void SetNewTime(TimePicker timePicker)
        {
            if (timePicker._isUpdatingTime) return;
            timePicker.SelectedTime = new TimeSpan(
                0,
                timePicker.SelectedHour,
                timePicker.SelectedMinute,
                timePicker.SelectedSecond,
                timePicker.SelectedMillisecond);
        }

        bool _isUpdatingTime;
        //call this method while updating the SelectedTimeProperty from the control itself only
        void BeginUpdateSelectedTime() { _isUpdatingTime = true; }

        void EndUpdateSelectedTime() { _isUpdatingTime = false; }

        void OnTimeSelectedChanged(TimeSpan newTime, TimeSpan oldTime)
        {
            var args = new TimeSelectedChangedRoutedEventArgs(SelectedTimeChangedEvent) { NewTime = newTime, OldTime = oldTime };
            RaiseEvent(args);
        }

        /// <summary>
        ///   Validates the string passed by parsing it as int and checking if it is inside the bounds specified then the resulting int will be incremented/decremented
        /// </summary>
        /// <param name="num"> The string to parse as int and increment/decrement </param>
        /// <param name="minValue"> The min value for the bound checking </param>
        /// <param name="maxVal"> The max value for the bounds checking </param>
        /// <param name="increment"> Pass true to increment and false to decrement </param>
        /// <returns> Returns the new number incremented or decremeneted </returns>
        public static int IncrementDecrementNumber(string num, int minValue, int maxVal, bool increment)
        {
            var newNum = ValidateNumber(num, minValue, maxVal);
            newNum = increment ? Math.Min(newNum + 1, maxVal) : Math.Max(newNum - 1, 0);
            return newNum;
        }

        /// <summary>
        ///   Parses the number and makes sure that it is within the bounds specified
        /// </summary>
        /// <param name="newNum"> The string to parse and validate </param>
        /// <param name="minValue"> The min value for the bound checking </param>
        /// <param name="maxValue"> The max value for the bound checking </param>
        /// <returns> Returns the int that was constructed from the string + bound checking </returns>
        public static int ValidateNumber(string newNum, int minValue, int maxValue)
        {
            int num;
            if (!int.TryParse(newNum, out num)) return 0;

            return ValidateNumber(num, minValue, maxValue);
        }

        /// <summary>
        ///   makes sure that the number is within the bounds specified
        /// </summary>
        /// <param name="newNum"> The number to validate </param>
        /// <param name="minValue"> The min value for the bound checking </param>
        /// <param name="maxValue"> The max value for the bound checking </param>
        /// <returns> Returns the int that was constructed from the string + bound checking </returns>
        public static int ValidateNumber(int newNum, int minValue, int maxValue)
        {
            newNum = Math.Max(newNum, minValue);
            newNum = Math.Min(newNum, maxValue);

            return newNum;
        }
        #endregion
    }

    #region Routed Event
    /// <summary>
    ///   Delegate for the TimeSelectedChanged event
    /// </summary>
    /// <param name="sender"> The object raising the event </param>
    /// <param name="e"> The routed event arguments </param>
    public delegate void TimeSelectedChangedEventHandler(object sender, TimeSelectedChangedRoutedEventArgs e);

    /// <summary>
    ///   Routed event arguments for the TimeSelectedChanged event
    /// </summary>
    public class TimeSelectedChangedRoutedEventArgs : RoutedEventArgs
    {
        /// <summary>
        ///   Gets or sets the new time
        /// </summary>
        public TimeSpan NewTime { get; set; }

        /// <summary>
        ///   Gets or sets the old time
        /// </summary>
        public TimeSpan OldTime { get; set; }

        /// <summary>
        ///   Constructor
        /// </summary>
        /// <param name="routedEvent"> The event that is raised </param>
        public TimeSelectedChangedRoutedEventArgs(RoutedEvent routedEvent)
            : base(routedEvent) { }
    }
    #endregion
}
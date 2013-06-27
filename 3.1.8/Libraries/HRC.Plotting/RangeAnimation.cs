using System;
using System.Windows;
using System.Windows.Media.Animation;

namespace HRC.Plotting
{
    public class RangeAnimation : AnimationTimeline
    {
        #region dependency property Range From

        public static DependencyProperty FromProperty = DependencyProperty.Register("From",
                                                                                    typeof(Range),
                                                                                    typeof(RangeAnimation),
                                                                                    new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, FromPropertyChanged));

        public Range From { get { return (Range)GetValue(FromProperty); } set { SetValue(FromProperty, value); } }

        static void FromPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((RangeAnimation)obj).FromPropertyChanged(); }
        void FromPropertyChanged() { _from = new Range(From); }
        #endregion

        #region dependency property Range To

        public static DependencyProperty ToProperty = DependencyProperty.Register("To",
                                                                                  typeof(Range),
                                                                                  typeof(RangeAnimation),
                                                                                  new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, ToPropertyChanged));

        public Range To { get { return (Range)GetValue(ToProperty); } set { SetValue(ToProperty, value); } }

        static void ToPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((RangeAnimation)obj).ToPropertyChanged(); }
        void ToPropertyChanged() { }
        #endregion

        #region dependency property IEasingFunction EasingFunction

        public static DependencyProperty EasingFunctionProperty = DependencyProperty.Register("EasingFunction",
                                                                                              typeof(IEasingFunction),
                                                                                              typeof(RangeAnimation),
                                                                                              new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, EasingFunctionPropertyChanged));

        public IEasingFunction EasingFunction { get { return (IEasingFunction)GetValue(EasingFunctionProperty); } set { SetValue(EasingFunctionProperty, value); } }

        static void EasingFunctionPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((RangeAnimation)obj).EasingFunctionPropertyChanged(); }
        void EasingFunctionPropertyChanged() { }
        #endregion

        public RangeAnimation() {}
        public RangeAnimation(Range from, Range to, Duration duration, FillBehavior fillBehavior = FillBehavior.Stop, IEasingFunction easingFunction = null)
        {
            From = from;
            To = to;
            Duration = duration;
            FillBehavior = fillBehavior;
            EasingFunction = easingFunction;
        }

        Range _from;
        protected override Freezable CreateInstanceCore() { return new RangeAnimation(); }
        public override Type TargetPropertyType { get { return typeof(Range); } }
        public override object GetCurrentValue(object defaultOriginValue, object defaultDestinationValue, AnimationClock animationClock)
        {
            if (!animationClock.CurrentProgress.HasValue) return From;

            var progress = (EasingFunction != null) ? EasingFunction.Ease(animationClock.CurrentProgress.Value) : animationClock.CurrentProgress.Value;

            double newMin, newMax;
            if (_from.Min > To.Min) newMin = (1 - progress) * (_from.Min - To.Min) + To.Min;
            else newMin = progress * (To.Min - _from.Min) + _from.Min;
            if (_from.Max > To.Max) newMax = (1 - progress) * (_from.Max - To.Max) + To.Max;
            else newMax = progress * (To.Max - _from.Max) + _from.Max;
            From.Update(newMin, newMax);
            //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} GetCurrentValue returning: {1}", DateTime.Now, From));
            return From;
        }
    }
}
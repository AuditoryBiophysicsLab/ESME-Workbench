using System.Windows;

namespace HRC.WPF
{
    public class ViewPropertyBinding
    {
        #region DataPipes (Attached DependencyProperty)

        public static readonly DependencyProperty ViewPropertyBindingsProperty =
            DependencyProperty.RegisterAttached("ViewPropertyBindings",
            typeof(ViewPropertyCollection),
            typeof(ViewPropertyBinding),
            new UIPropertyMetadata(null));

        public static void SetViewPropertyBindings(DependencyObject o, ViewPropertyCollection value)
        {
            o.SetCurrentValue(ViewPropertyBindingsProperty, value);
        }

        public static ViewPropertyCollection GetViewPropertyBindings(DependencyObject o)
        {
            return (ViewPropertyCollection)o.GetValue(ViewPropertyBindingsProperty);
        }

        #endregion
    }

    public class ViewPropertyCollection : FreezableCollection<ViewProperty>
    {

    }

    public class ViewProperty : Freezable
    {
        #region Source (DependencyProperty)

        public object Source
        {
            get { return GetValue(SourceProperty); }
            set { SetCurrentValue(SourceProperty, value); }
        }
        public static readonly DependencyProperty SourceProperty =
            DependencyProperty.Register("Source", typeof(object), typeof(ViewProperty),
            new FrameworkPropertyMetadata(null, new PropertyChangedCallback(OnSourceChanged)));

        private static void OnSourceChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            ((ViewProperty)d).OnSourceChanged(e);
        }

        protected virtual void OnSourceChanged(DependencyPropertyChangedEventArgs e)
        {
            Target = e.NewValue;
        }

        #endregion

        #region Target (DependencyProperty)

        public object Target
        {
            get { return GetValue(TargetProperty); }
            set { SetCurrentValue(TargetProperty, value); }
        }
        public static readonly DependencyProperty TargetProperty =
            DependencyProperty.Register("Target", typeof(object), typeof(ViewProperty),
            new FrameworkPropertyMetadata(null));

        #endregion

        protected override Freezable CreateInstanceCore()
        {
            return new ViewProperty();
        }
    }
}

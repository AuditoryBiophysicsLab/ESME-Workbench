using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Windows;

namespace HRC.WPF
{
    public class DataPiping
    {
        #region DataPipes (Attached DependencyProperty)

        public static readonly DependencyProperty DataPipesProperty =
            DependencyProperty.RegisterAttached("DataPipes",
            typeof(DataPipeCollection),
            typeof(DataPiping),
            new UIPropertyMetadata(null));

        public static void SetDataPipes(DependencyObject o, DataPipeCollection value)
        {
            o.SetValue(DataPipesProperty, value);
        }

        public static DataPipeCollection GetDataPipes(DependencyObject o)
        {
            return (DataPipeCollection)o.GetValue(DataPipesProperty);
        }

        #endregion
    }

    public class DataPipeCollection : FreezableCollection<DataPipe>
    {

    }

    public class DataPipe : Freezable
    {
        #region Source (DependencyProperty)

        public object Source
        {
            get { return GetValue(SourceProperty); }
            set { SetValue(SourceProperty, value); }
        }
        public static readonly DependencyProperty SourceProperty =
            DependencyProperty.Register("Source", typeof(object), typeof(DataPipe),
            new FrameworkPropertyMetadata(null, OnSourceChanged));

        private static void OnSourceChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            ((DataPipe)d).OnSourceChanged(e);
        }

        protected virtual void OnSourceChanged(DependencyPropertyChangedEventArgs e)
        {
            //if (ShowDebugMessages && e.NewValue is IRange) Debug.WriteLine("{0:HH:mm:ss.fff} SourceChanged: Range changed from {1} to {2}", DateTime.Now, (IRange)e.OldValue, (IRange)e.NewValue);
            Target = e.NewValue;
        }
        #endregion

        #region ShowDebugMessages (DependencyProperty)

        public bool ShowDebugMessages
        {
            get { return (bool)GetValue(ShowDebugMessagesProperty); }
            set { SetValue(ShowDebugMessagesProperty, value); }
        }
        public static readonly DependencyProperty ShowDebugMessagesProperty =
            DependencyProperty.Register("ShowDebugMessages", typeof(bool), typeof(DataPipe),
            new FrameworkPropertyMetadata(false));

        #endregion

        #region Target (DependencyProperty)

        public object Target
        {
            get { return GetValue(TargetProperty); }
            set { SetValue(TargetProperty, value); }
        }
        public static readonly DependencyProperty TargetProperty =
            DependencyProperty.Register("Target", typeof(object), typeof(DataPipe),
            new FrameworkPropertyMetadata(null));

        #endregion

        protected override Freezable CreateInstanceCore()
        {
            return new DataPipe();
        }
    }
}

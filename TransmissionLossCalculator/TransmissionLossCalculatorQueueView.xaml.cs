using System;
using System.ComponentModel;
using System.Windows;
using System.Windows.Input;

namespace TransmissionLossCalculator
{
    /// <summary>
    ///   Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class TransmissionLossQueueView
    {
        WindowState _lastWindowState;
        bool _shouldClose;

        public TransmissionLossQueueView() { InitializeComponent(); }
        protected override void OnStateChanged(EventArgs e)
        {
            _lastWindowState = WindowState;
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            if (_shouldClose) return;
            e.Cancel = true;
            Hide();
        }

        private void OnNotificationAreaIconDoubleClick(object sender, MouseButtonEventArgs e)
        {
            if (e.ChangedButton == MouseButton.Left)
            {
                Open();
            }
        }

        private void OnMenuItemOpenClick(object sender, EventArgs e)
        {
            Open();
        }

        private void Open()
        {
            Show();
            WindowState = _lastWindowState;
        }

        private void OnMenuItemExitClick(object sender, EventArgs e)
        {
            _shouldClose = true;
            Properties.Settings.Default.Save();
            Close();
        }

    }
}
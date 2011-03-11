using System;
using System.Runtime.InteropServices;
using System.Windows.Interop;
using Cinch;

namespace ESMEWorkBench.Views
{
    /// <summary>
    /// Interaction logic for ScenarioSimulatorOptionsView.xaml
    /// </summary>
    [PopupNameToViewLookupKeyMetadata("ScenarioSimulatorOptionsView", typeof (ScenarioSimulatorOptionsView))]
    public partial class ScenarioSimulatorOptionsView
    {
        private const int GWL_STYLE = -16;
        private const int WS_SYSMENU = 0x80000;
        [DllImport("user32.dll", SetLastError = true)]
        private static extern int GetWindowLong(IntPtr hWnd, int nIndex);
        [DllImport("user32.dll")]
        private static extern int SetWindowLong(IntPtr hWnd, int nIndex, int dwNewLong);

        public ScenarioSimulatorOptionsView() { InitializeComponent(); }

        private void OnLoaded(object sender, System.Windows.RoutedEventArgs e)
        {
            var hwnd = new WindowInteropHelper(this).Handle;
            SetWindowLong(hwnd, GWL_STYLE, GetWindowLong(hwnd, GWL_STYLE) & ~WS_SYSMENU);

        }
    }
}
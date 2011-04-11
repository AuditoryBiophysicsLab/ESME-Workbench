using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using ESME.Data;

namespace ESMEWorkBench.Views
{
    /// <summary>
    /// Interaction logic for ScenarioSimulatorUserControlView.xaml
    /// </summary>
    public partial class ScenarioSimulatorUserControlView : UserControl
    {
        public ScenarioSimulatorUserControlView()
        {
            InitializeComponent();
        }

        public static DependencyProperty ScenarioSimulatorSettingsProperty = DependencyProperty.Register("ScenarioSimulatorSettings", typeof (ScenarioSimulatorSettings), typeof (ScenarioSimulatorUserControlView), new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public ScenarioSimulatorSettings ScenarioSimulatorSettings
        {
            get { return (ScenarioSimulatorSettings) GetValue(ScenarioSimulatorSettingsProperty); }
            set { SetValue(ScenarioSimulatorSettingsProperty,value); }
        }
    }
}

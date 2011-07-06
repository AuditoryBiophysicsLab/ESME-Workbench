using System.Collections.Generic;
using ESME.Environment.NAVO;

namespace ESME.Views.Controls
{
    /// <summary>
    /// Interaction logic for TimePeriodComboBox.xaml
    /// </summary>
    public partial class TimePeriodComboBox
    {
        public TimePeriodComboBox()
        {
            InitializeComponent();
        }

        public List<NAVOTimePeriod> ListItems
        {
            get { return _listItems ?? (_listItems = new List<NAVOTimePeriod> { NAVOTimePeriod.January, }); }
        }

        private static List<NAVOTimePeriod> _listItems;
    }
}

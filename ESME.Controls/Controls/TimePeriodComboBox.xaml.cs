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
            get { return _listItems ?? (_listItems = new List<NAVOTimePeriod>
                                                         {
                                                             NAVOTimePeriod.January,
                                                             NAVOTimePeriod.February,
                                                             NAVOTimePeriod.March,
                                                             NAVOTimePeriod.April,
                                                             NAVOTimePeriod.May,
                                                             NAVOTimePeriod.June,
                                                             NAVOTimePeriod.July,
                                                             NAVOTimePeriod.August,
                                                             NAVOTimePeriod.September,
                                                             NAVOTimePeriod.October,
                                                             NAVOTimePeriod.November,
                                                             NAVOTimePeriod.December,
                                                             NAVOTimePeriod.Spring,
                                                             NAVOTimePeriod.Summer,
                                                             NAVOTimePeriod.Fall,
                                                             NAVOTimePeriod.Winter,
                                                             NAVOTimePeriod.Cold,
                                                             NAVOTimePeriod.Warm,
                                                         }); }
        }

        private static List<NAVOTimePeriod> _listItems;
    }
}

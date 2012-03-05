using System.Collections.Generic;
using ESME.Environment;

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

        public List<TimePeriod> ListItems
        {
            get { return _listItems ?? (_listItems = new List<TimePeriod>
                                                         {
                                                             TimePeriod.January,
                                                             TimePeriod.February,
                                                             TimePeriod.March,
                                                             TimePeriod.April,
                                                             TimePeriod.May,
                                                             TimePeriod.June,
                                                             TimePeriod.July,
                                                             TimePeriod.August,
                                                             TimePeriod.September,
                                                             TimePeriod.October,
                                                             TimePeriod.November,
                                                             TimePeriod.December,
                                                             TimePeriod.Spring,
                                                             TimePeriod.Summer,
                                                             TimePeriod.Fall,
                                                             TimePeriod.Winter,
                                                             TimePeriod.Cold,
                                                             TimePeriod.Warm,
                                                         }); }
        }

        private static List<TimePeriod> _listItems;
    }
}

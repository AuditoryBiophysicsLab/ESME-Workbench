using System.Windows;
using System.Windows.Controls.Primitives;
using Microsoft.Windows.Controls.Ribbon;

namespace ESME.Views.Controls
{
    public class SlimRibbonApplicationMenu : RibbonApplicationMenu
    {
        private const double DefaultPopupWidth = 180;

        public double PopupWidth
        {
            get { return (double)GetValue(PopupWidthProperty); }
            set { SetValue(PopupWidthProperty, value); }
        }

        public static readonly DependencyProperty PopupWidthProperty =
            DependencyProperty.Register("PopupWidth", typeof(double),
            typeof(SlimRibbonApplicationMenu), new UIPropertyMetadata(DefaultPopupWidth));


        public override void OnApplyTemplate()
        {
            base.OnApplyTemplate();
            DropDownOpened += (s, e) =>
            {
                var popupObj = GetTemplateChild("PART_Popup");
                var popupPanel = (Popup)popupObj;
                if (popupPanel != null) popupPanel.Width = (double)GetValue(PopupWidthProperty);
            };
        }
    }
}

using System.Windows.Interactivity;
using System;
using System.ComponentModel;
using System.Windows;
using System.Windows.Navigation;

namespace HRC.Interactivity
{
    public class HyperlinkAction : TargetedTriggerAction<FrameworkElement>
    {
        #region Properties to Expose
        [Category("Hyperlink Properties")]
        public string Url
        {
            get { return (string)GetValue(UrlProperty); }
            set { SetValue(UrlProperty, value); }
        }

        public static readonly DependencyProperty UrlProperty = DependencyProperty.Register("Hyperlink URL", typeof(string), typeof(HyperlinkAction), new PropertyMetadata("http://www.live.com"));

        [Category("Hyperlink Properties")]
        public string TargetURL
        {
            get { return (string)GetValue(TargetURLProperty); }
            set { SetValue(TargetURLProperty, value); }
        }

        public static readonly DependencyProperty TargetURLProperty = DependencyProperty.Register("TargetURL", typeof(string), typeof(HyperlinkAction), new PropertyMetadata("_self"));


        #endregion

        protected override void Invoke(object o)
        {
            var service = NavigationService.GetNavigationService(Application.Current.MainWindow);
            if (service != null)
            {
                service.Navigate(new Uri(Url), Target != null ? TargetURL : "_self");
            }
        }
    }
}

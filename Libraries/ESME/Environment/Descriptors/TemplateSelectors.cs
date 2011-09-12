using System.Windows;
using System.Windows.Controls;

namespace ESME.Environment.Descriptors
{
    public class EnvironmentTreeTemplateSelector : DataTemplateSelector
    {
        public DataTemplate DataAvailableTemplate { get; set; }
        public DataTemplate DataNotAvailableTemplate { get; set; }

        public override DataTemplate SelectTemplate(object item, DependencyObject container)
        {
            var element = container as FrameworkElement;

            if (element == null) return null;
            if (item is SampleCountTreeItem)
                return ((SampleCountTreeItem)item).IsDataAvailable ? DataAvailableTemplate : DataNotAvailableTemplate;
            return null;
        }
    }

    public class GeoRectTemplateSelector : DataTemplateSelector
    {
        public DataTemplate NullTemplate { get; set; }
        public DataTemplate NotNullTemplate { get; set; }

        public override DataTemplate SelectTemplate(object item, DependencyObject container)
        {
            var element = container as FrameworkElement;

            if (element == null) return NullTemplate;
            if (item is SampleCountTreeItem) return NotNullTemplate;
            return null;
        }
    }
}

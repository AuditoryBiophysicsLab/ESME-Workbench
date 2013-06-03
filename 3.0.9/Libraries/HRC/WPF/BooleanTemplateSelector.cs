using System.Windows;
using System.Windows.Controls;

namespace HRC.WPF
{
    public class BooleanTemplateSelector : DataTemplateSelector
    {
        public DataTemplate TrueTemplate { get; set; }
        public DataTemplate FalseTemplate { get; set; }

        public override DataTemplate SelectTemplate(object item, DependencyObject container)
        {
            var conditional = item as bool?;
            if (conditional == null) return null;
            return conditional.Value ? TrueTemplate : FalseTemplate;
        }
    }
}

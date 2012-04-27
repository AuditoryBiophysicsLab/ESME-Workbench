using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;

namespace HRC.WPF
{
#if false
    public class IsMouseOverTemplateSelector : DataTemplateSelector
    {
        public DataTemplate MouseOverTemplate { get; set; }
        public DataTemplate NormalTemplate { get; set; }

        public override DataTemplate SelectTemplate(object item, DependencyObject container)
        {
            if (!(item is TreeViewItem)) return NormalTemplate;
            String path = (string)item;
            String ext = System.IO.Path.GetExtension(path);
            if (System.IO.File.Exists(path) && ext == ".jpg")
                return ImageTemplate;
            return StringTemplate;
        }
    }
#endif
}

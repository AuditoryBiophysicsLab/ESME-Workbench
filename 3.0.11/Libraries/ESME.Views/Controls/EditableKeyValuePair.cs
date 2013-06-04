using System.Windows;
using System.Windows.Controls;

namespace ESME.Views.Controls
{
    public class EditableKeyValuePair<TKey, TValue> : IIsEditable
    {
        public TKey Key { get; set; }
        public TValue Value { get; set; }
        public bool IsEditable { get; set; }

        public EditableKeyValuePair() { }

        public EditableKeyValuePair(TKey key, TValue value, bool isEditable = false)
        {
            Key = key;
            Value = value;
            IsEditable = isEditable;
        }
    }

    public class EditableKeyValuePairTemplateSelector : DataTemplateSelector
    {
        public DataTemplate TrueTemplate { get; set; }
        public DataTemplate FalseTemplate { get; set; }

        public override DataTemplate SelectTemplate(object item, DependencyObject container)
        {
            var isEditable = item as IIsEditable;
            if (isEditable == null) return null;
            return isEditable.IsEditable ? TrueTemplate : FalseTemplate;
        }
    }

    internal interface IIsEditable
    {
        bool IsEditable { get; set; }
    }
}
using System;

namespace HRC.ViewModels
{
    [AttributeUsage(AttributeTargets.Class)]
    public class PopupNameToViewLookupKeyMetadataAttribute : Attribute
    {
        public string PopupName { get; private set; }
        public Type ViewLookupKey { get; private set; }

        public PopupNameToViewLookupKeyMetadataAttribute(string popupName, Type viewLookupKey)
        {
            PopupName = popupName;
            ViewLookupKey = viewLookupKey;
        }

    }

    [AttributeUsage(AttributeTargets.Class)]
    public class ViewnameToViewLookupKeyMetadataAttribute : Attribute
    {
        public string ViewName { get; private set; }
        public Type ViewLookupKey { get; private set; }

        public ViewnameToViewLookupKeyMetadataAttribute(string viewName, Type viewLookupKey)
        {
            ViewName = viewName;
            ViewLookupKey = viewLookupKey;
        }
    }
}

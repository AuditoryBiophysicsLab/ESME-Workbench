using System.Windows;

namespace ESME
{
    public class Configuration
    {
        static Configuration()
        {
#if IS_CLASSIFIED_MODEL
            IsClassifiedModel = true;
            ClassifiedVisibility = Visibility.Visible;
            IsUnclassifiedModel = false;
            UnclassifiedVisibility = Visibility.Collapsed;
#else
            IsClassifiedModel = false;
            ClassifiedVisibility = Visibility.Collapsed;
            IsUnclassifiedModel = true;
            UnclassifiedVisibility = Visibility.Visible;
#endif
        }

        public static bool IsClassifiedModel { get; private set; }
        public static Visibility ClassifiedVisibility { get; private set; }
        public static bool IsUnclassifiedModel { get; private set; }
        public static Visibility UnclassifiedVisibility { get; private set; }
    }
}

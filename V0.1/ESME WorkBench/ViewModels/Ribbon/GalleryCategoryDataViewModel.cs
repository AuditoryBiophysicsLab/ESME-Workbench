using System.Collections.ObjectModel;
using System.ComponentModel;

namespace ESMERibbonDemo.ViewModels.Ribbon
{
    public class GalleryCategoryDataViewModel<T> : ControlDataViewModel
    {
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Content)]
        public ObservableCollection<T> GalleryItemDataCollection
        {
            get
            {
                if (_controlDataCollection == null)
                {
                    _controlDataCollection = new ObservableCollection<T>();
                }
                return _controlDataCollection;
            }
        }
        private ObservableCollection<T> _controlDataCollection;
    }
}

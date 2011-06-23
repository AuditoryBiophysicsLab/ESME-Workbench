﻿using System.Collections.ObjectModel;
using System.ComponentModel;

namespace ESMEWorkBench.ViewModels.Ribbon
{
    public class GalleryCategoryDataViewModel<T> : ControlDataViewModel
    {
        ObservableCollection<T> _controlDataCollection;

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Content)]
        public ObservableCollection<T> GalleryItemDataCollection
        {
            get { return _controlDataCollection ?? (_controlDataCollection = new ObservableCollection<T>()); }
        }
    }
}
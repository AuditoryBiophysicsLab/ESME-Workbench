using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
using Cinch;

namespace ESME.Views.Locations
{
    /// <summary>
    /// Interaction logic for MetadataPropertiesView.xaml
    /// </summary>
   [PopupNameToViewLookupKeyMetadata("MetadataPropertiesView", typeof(MetadataPropertiesView))]
    public partial class MetadataPropertiesView 
    {
        public MetadataPropertiesView()
        {
            InitializeComponent();
        }
    }
}

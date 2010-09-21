using System.Collections.ObjectModel;
using System.Linq;

namespace ESMEWorkBench.ViewModels.Layers
{
    public class LayersCollection : ObservableCollection<LayerViewModel>
    {
        public LayerViewModel this[string name]
        {
            get { return this.FirstOrDefault(cur => cur.LayerName == name); }
        }
    }
}